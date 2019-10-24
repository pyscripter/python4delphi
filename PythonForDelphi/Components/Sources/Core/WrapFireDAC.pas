{$REGION 'Licence'}
{
   Wrapper classes for FireDec TFDTable and TFDQuery
   Original Code by https://github.com/hartmutdavid
==============================================================================}
{$ENDREGION}
unit WrapFireDAC;

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

{
   Published properties and events are supported automaitcally
   by the WrapDelphi infrastructure.  There is no need
   to exposed them here.
}

TPyDBCommon = class(TPyDelphiComponent)
  function  GetProperties : PPyObject;
  procedure AppendProperties( List : PPyObject ); virtual;
  procedure AppendProp( List : PPyObject; const prop : String );
  function  GetAttrO(key: PPyObject) : PPyObject; override;
  procedure RaiseDBError( E : Exception );
end;

TPyDBField = class(TPyDBCommon)
private
  function  GetDelphiObject: TField;
  procedure SetDelphiObject(const Value: TField);
public
  function  GetAttrO(key: PPyObject) : PPyObject; override;
  function  SetAttrO(key, value: PPyObject) : Integer; override;

  class function  DelphiObjectClass : TClass; override;
  class procedure RegisterMethods( PythonType : TPythonType ); override;

  function  CheckField : Boolean;
  procedure AppendProperties( List : PPyObject ); override;

  // Do-Methods
  function Do_Clear( args : PPyObject ) : PPyObject; cdecl;
  function Do_FocusControl( args : PPyObject ) : PPyObject; cdecl;
  function Do_IsValidChar( args : PPyObject ) : PPyObject; cdecl;
  function Do_RefreshLookupList( args : PPyObject ) : PPyObject; cdecl;

  property DelphiObject: TField read GetDelphiObject write SetDelphiObject;
end;

TDSRowsAccess = class(TContainerAccess)
private
  function  GetContainer: TFDDataset;
public
  function GetItem(AIndex : Integer) : PPyObject; override;
  function GetSize : Integer; override;

  class function ExpectedContainerClass : TClass; override;
  class function Name : String; override;
end;

TPyDBDataset = class (TPyDelphiComponent)
private
  function  GetDelphiObject: TFDDataset;
  procedure SetDelphiObject(const Value: TFDDataset);
protected
  procedure RaiseDBError( E : Exception );
public
  // Class methodes
  class function  DelphiObjectClass : TClass; override;
  class function  GetContainerAccessClass : TContainerAccessClass; override;
  class procedure RegisterGetSets( PythonType : TPythonType ); override;
  class procedure RegisterMethods( PythonType : TPythonType ); override;
  class procedure SetupType( PythonType : TPythonType ); override;
  //
  // Property Getters
  function Get_RowsCount( AContext : Pointer) : PPyObject; cdecl;
  function Get_Rows( AContext : Pointer) : PPyObject; cdecl;
  //
  // Attributes
  function  GetAttrO(key: PPyObject) : PPyObject; override;
  function  SetAttrO(key, value: PPyObject) : Integer; override;
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

TPyDBTable = class (TPyDBDataset)
private
  function CheckActiveDBTable(aMustOpen: Boolean): Boolean;
public
  destructor Destroy; override;

  function  GetDelphiObject: TFDTable;
  procedure SetDelphiObject(const Value: TFDTable);
  //
  class function  DelphiObjectClass : TClass; override;
  class procedure RegisterMethods( PythonType : TPythonType ); override;
  // Attributes
  function  GetAttrO(key: PPyObject) : PPyObject; override;
  function  SetAttrO(key, value: PPyObject) : Integer; override;
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

  property DelphiObject: TFDTable read GetDelphiObject write SetDelphiObject;
end;

TPyDBQuery = class (TPyDBDataset)
private
  function CheckActiveDBQuery(aMustOpen: Boolean): Boolean;
protected
public
  destructor Destroy; override;

  function  GetDelphiObject: TFDQuery;
  procedure SetDelphiObject(const Value: TFDQuery);
  //
  class function  DelphiObjectClass : TClass; override;
  class procedure RegisterMethods( PythonType : TPythonType ); override;
  // Attributes
  function  GetAttrO(key: PPyObject) : PPyObject; override;
  function  SetAttrO(key, value: PPyObject) : Integer; override;
  // Methods
  function Do_Open( args : PPyObject ) : PPyObject; cdecl;
  function Do_Close( args : PPyObject ) : PPyObject; cdecl;
  function Do_Prepare( args : PPyObject ) : PPyObject; cdecl;
  function Do_Unprepare( args : PPyObject ) : PPyObject; cdecl;
  function Do_ExecSQL( args : PPyObject ) : PPyObject; cdecl;
  //
  property DelphiObject: TFDQuery read GetDelphiObject write SetDelphiObject;
end;

TPyFireDACRegistration = class(TRegisteredUnit)
public
  function Name(): String; override;
  procedure RegisterWrappers(aPyDelphiWrapper: TPyDelphiWrapper); override;
  procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); override;
end;

function SqlTimeToVarDate(V : Variant) : Variant;

implementation

Uses
  Data.SqlTimSt;
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
   obj := PyUnicode_FromWideString(prop);
   PyList_Append( List, obj );
   Py_XDecRef(obj);
 end;
end;

function  TPyDBCommon.GetAttrO(key: PPyObject) : PPyObject;
Var
  SKey : string;
begin
 try
   SKey := LowerCase(GetPythonEngine.PyString_AsDelphiString(Key));
   if (SKey = '__properties__' ) or
      (SKey = '__members__' ) then
     Result := GetProperties
   else
     Result := inherited GetAttrO(key);
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

// ----------------------- TPyDBField ------------------------------------------

class function TPyDBField.DelphiObjectClass: TClass;
begin
  Result := TField;
end;


// Then we override the needed services

function  TPyDBField.GetAttrO(key: PPyObject) : PPyObject;
{ TODO : Remove published properties }
var
 l_sUpperKey: String;
begin
 with GetPythonEngine do begin
   if not CheckField then begin
     Result := nil;
     Exit;
   end;
   try
     l_sUpperKey := UpperCase(PyString_AsDelphiString(Key));
     if l_sUpperKey = 'ALIGNMENT' then
       Result := VariantAsPyObject( Integer(DelphiObject.Alignment) )
     else if l_sUpperKey = 'ASBOOLEAN' then
       Result := VariantAsPyObject( DelphiObject.AsBoolean )
     else if l_sUpperKey = 'ASDATETIME' then
       Result := VariantAsPyObject( DelphiObject.AsDateTime )
     else if l_sUpperKey = 'ASFLOAT' then
       Result := VariantAsPyObject( DelphiObject.AsFloat )
     else if l_sUpperKey = 'ASINTEGER' then
       Result := VariantAsPyObject( DelphiObject.AsInteger )
     else if l_sUpperKey = 'ASSTRING' then
       Result := VariantAsPyObject( DelphiObject.AsString )
     else if l_sUpperKey = 'CANMODIFY' then
       Result := VariantAsPyObject( DelphiObject.CanModify )
     else if l_sUpperKey = 'ConstraintErrorMessage' then
       Result := VariantAsPyObject( DelphiObject.ConstraintErrorMessage )
     else if l_sUpperKey = 'CurValue' then
       Result := VariantAsPyObject( DelphiObject.CurValue )
     else if l_sUpperKey = 'CustomConstraint' then
       Result := VariantAsPyObject( DelphiObject.CustomConstraint )
     else if l_sUpperKey = 'DATASIZE' then
       Result := VariantAsPyObject( DelphiObject.DataSize )
     else if l_sUpperKey = 'DATATYPE' then
       Result := VariantAsPyObject( DelphiObject.DataType )
     else if l_sUpperKey = 'DefaultExpression' then
       Result := VariantAsPyObject( DelphiObject.DefaultExpression )
     else if l_sUpperKey = 'DISPLAYLABEL' then
       Result := VariantAsPyObject( DelphiObject.DisplayLabel )
     else if l_sUpperKey = 'DISPLAYNAME' then
       Result := VariantAsPyObject( DelphiObject.DisplayName )
     else if l_sUpperKey = 'DISPLAYTEXT' then
       Result := VariantAsPyObject( DelphiObject.DisplayText )
     else if l_sUpperKey = 'DISPLAYWIDTH' then
       Result := VariantAsPyObject( DelphiObject.DisplayWidth )
     else if l_sUpperKey = 'EDITMASK' then
       Result := VariantAsPyObject( DelphiObject.EditMask )
     else if l_sUpperKey = 'FIELDKIND' then
       Result := VariantAsPyObject( DelphiObject.FieldKind )
     else if l_sUpperKey = 'FIELDNAME' then
       Result := VariantAsPyObject( DelphiObject.FieldName)
     else if l_sUpperKey = 'FIELDNO' then
       Result := VariantAsPyObject( DelphiObject.FieldNo )
     else if l_sUpperKey = 'HASCONSTRAINTS' then
       Result := VariantAsPyObject( DelphiObject.HasConstraints )
     else if l_sUpperKey = 'IMPORTEDCONSTRAINT' then
       Result := VariantAsPyObject( DelphiObject.ImportedConstraint )
     else if l_sUpperKey = 'INDEX' then
       Result := VariantAsPyObject( DelphiObject.Index )
     else if l_sUpperKey = 'ISBLOB' then
       Result := VariantAsPyObject( DelphiObject.IsBlob)
     else if l_sUpperKey = 'ISINDEXFIELD' then
       Result := VariantAsPyObject( DelphiObject.IsIndexField )
     else if l_sUpperKey = 'ISNULL' then
       Result := VariantAsPyObject( DelphiObject.IsNull )
     else if l_sUpperKey = 'KEYFFIELDS' then
       Result := VariantAsPyObject( DelphiObject.KeyFields )
     else if l_sUpperKey = 'LOOKUP' then
       Result := VariantAsPyObject( DelphiObject.Lookup )
     else if l_sUpperKey = 'LOOKUPCACHE' then
       Result := VariantAsPyObject( DelphiObject.LookupCache )
     else if l_sUpperKey = 'LOOKUPKeyFields' then
       Result := VariantAsPyObject( DelphiObject.LookupKeyFields )
     else if l_sUpperKey = 'LOOKUPLIST' then
       Result := ReturnNone
     else if l_sUpperKey = 'LOOKUPRESULTFIELD' then
       Result := VariantAsPyObject( DelphiObject.LookupResultField )
     else if l_sUpperKey = 'NEWVALUE' then
       Result := VariantAsPyObject( DelphiObject.NewValue )
     else if l_sUpperKey = 'OFFSET' then
       Result := VariantAsPyObject( DelphiObject.Offset )
     else if l_sUpperKey = 'OLDVALUE' then
       Result := VariantAsPyObject( DelphiObject.OldValue )
     else if l_sUpperKey = 'ORIGIN' then
       Result := VariantAsPyObject( DelphiObject.Origin )
     else if l_sUpperKey = 'READONLY' then
       Result := VariantAsPyObject( DelphiObject.ReadOnly )
     else if l_sUpperKey = 'REQUIRED' then
       Result := VariantAsPyObject( DelphiObject.Required )
     else if l_sUpperKey = 'SIZE' then
       Result := VariantAsPyObject( DelphiObject.Size )
     else if l_sUpperKey = 'TEXT' then
       Result := VariantAsPyObject( DelphiObject.Text )
     else if l_sUpperKey = 'VALIDCHARS' then
       Result := ReturnNone
     else if l_sUpperKey = 'VALUE' then begin
       Result := VariantAsPyObject( SqlTimeToVarDate(DelphiObject.Value) );
     end
     else if l_sUpperKey = 'VISIBLE' then
       Result := VariantAsPyObject( DelphiObject.Visible )
     else
       Result := inherited GetAttrO(key);
   except
     on E : Exception do begin
       RaiseDBError( E );
       Result := nil;
     end;
   end;
 end;
end;

function TPyDBField.GetDelphiObject: TField;
begin
 Result := TField(inherited DelphiObject);
end;

function  TPyDBField.SetAttrO(key, value: PPyObject) : Integer;
{ TODO : Remove published properties }
var
 l_sUpperKey: String;
begin
 Result := -1;
 with GetPythonEngine do begin
   if not CheckField then
     Exit;
   try
     l_sUpperKey := UpperCase(PyString_AsDelphiString(Key));
     if l_sUpperKey = 'ALIGNMENT' then begin
       DelphiObject.Alignment := TAlignment(PyObjectAsVariant( value ));
       Result := 0;
     end
     else if l_sUpperKey = 'ASBOOLEAN' then begin
       DelphiObject.AsBoolean := PyObjectAsVariant( value );
       Result := 0;
     end
     else if l_sUpperKey = 'ASDATETIME' then begin
       DelphiObject.AsDateTime := PyObjectAsVariant( value );
       Result := 0;
     end
     else if l_sUpperKey = 'ASFLOAT' then begin
       DelphiObject.AsFloat := PyObjectAsVariant( value );
       Result := 0;
     end
     else if l_sUpperKey = 'ASINTEGER' then begin
       DelphiObject.AsInteger := PyObjectAsVariant( value );
       Result := 0;
     end
     else if l_sUpperKey = 'ASSTRING' then begin
       DelphiObject.AsString := PyObjectAsVariant( value );
       Result := 0;
     end
     else if l_sUpperKey = 'CANMODIFY' then begin
       Result := 0;
     end
     else if l_sUpperKey = 'CONSTRAINTERRORMESSAGE' then begin
       DelphiObject.ConstraintErrorMessage := PyObjectAsVariant( value );
       Result := 0;
     end
     else if l_sUpperKey = 'CURVALUE' then begin
       Result := 0;
     end
     else if l_sUpperKey = 'CUSTOMCONSTRAINT' then begin
       DelphiObject.CustomConstraint := PyObjectAsVariant( value );
       Result := 0;
     end
     else if l_sUpperKey = 'DATASIZE' then begin
       Result := 0;
     end
     else if l_sUpperKey = 'DATATYPE' then begin
       Result := 0;
     end
     else if l_sUpperKey = 'DEFAULTEXPRESSION' then begin
       DelphiObject.DefaultExpression := PyObjectAsVariant( value );
       Result := 0;
     end
     else if l_sUpperKey = 'DISPLAYLABEL' then begin
       DelphiObject.DisplayLabel := PyObjectAsVariant( value );
       Result := 0;
     end
     else if l_sUpperKey = 'DISPLAYNAME' then begin
       Result := 0;
     end
     else if l_sUpperKey = 'DISPLAYTEXT' then begin
       Result := 0;
     end
     else if l_sUpperKey = 'DISPLAYWIDTH' then begin
       DelphiObject.DisplayWidth := PyObjectAsVariant( value );
       Result := 0;
     end
     else if l_sUpperKey = 'EDITMASK' then begin
       DelphiObject.EditMask := PyObjectAsVariant( value );
       Result := 0;
     end
     else if l_sUpperKey = 'FIELDKIND' then begin
       DelphiObject.FieldKind := PyObjectAsVariant( value );
       Result := 0;
     end
     else if l_sUpperKey = 'FIELDNAME' then begin
       DelphiObject.FieldName:= PyObjectAsVariant( value );
       Result := 0;
     end
     else if l_sUpperKey = 'FIELDNO' then begin
       Result := 0;
     end
     else if l_sUpperKey = 'HASCONSTRAINTS' then begin
       Result := 0;
     end
     else if l_sUpperKey = 'IMPORTEDCONSTRAINT' then begin
       DelphiObject.ImportedConstraint := PyObjectAsVariant( value );
       Result := 0;
     end
     else if l_sUpperKey = 'INDEX' then begin
       DelphiObject.Index := PyObjectAsVariant( value );
       Result := 0;
     end
     else if l_sUpperKey = 'ISBLOB' then begin
       Result := 0;
     end
     else if l_sUpperKey = 'ISINDEXFIELD' then begin
       Result := 0;
     end
     else if l_sUpperKey = 'ISNULL' then begin
       Result := 0;
     end
     else if l_sUpperKey = 'KEYFIELDS' then begin
       DelphiObject.KeyFields := PyObjectAsVariant( value );
       Result := 0;
     end
     else if l_sUpperKey = 'LOOKUP' then begin
       DelphiObject.Lookup := PyObjectAsVariant( value );
       Result := 0;
     end
     else if l_sUpperKey = 'LOOKUPCACHE' then begin
       DelphiObject.LookupCache := PyObjectAsVariant( value );
       Result := 0;
     end
     else if l_sUpperKey = 'LOOKUPKEYFIELDS' then begin
       DelphiObject.LookupKeyFields := PyObjectAsVariant( value );
       Result := 0;
     end
     else if l_sUpperKey = 'LOOKUPLIST' then begin
       Result := 0;
     end
     else if l_sUpperKey = 'LOOKUPRESULTFIELD' then begin
       DelphiObject.LookupResultField := PyObjectAsVariant( value );
       Result := 0;
     end
     else if l_sUpperKey = 'NEWVALUE' then begin
       DelphiObject.NewValue := PyObjectAsVariant( value );
       Result := 0;
     end
     else if l_sUpperKey = 'OFFSET' then begin
       Result := 0;
     end
     else if l_sUpperKey = 'OLDVALUE' then begin
       Result := 0;
     end
     else if l_sUpperKey = 'ORIGIN' then begin
       DelphiObject.Origin := PyObjectAsVariant( value );
       Result := 0;
     end
     else if l_sUpperKey = 'READONLY' then begin
       DelphiObject.ReadOnly := PyObjectAsVariant( value );
       Result := 0;
     end
     else if l_sUpperKey = 'REQUIRED' then begin
       DelphiObject.Required := PyObjectAsVariant( value );
       Result := 0;
     end
     else if l_sUpperKey = 'SIZE' then begin
       DelphiObject.Size := PyObjectAsVariant( value );
       Result := 0;
     end
     else if l_sUpperKey = 'TEXT' then begin
       DelphiObject.Text := PyObjectAsVariant( value );
       Result := 0;
     end
     else if l_sUpperKey = 'VALIDCHARS' then begin
       Result := 0;
     end
     else if l_sUpperKey = 'VALUE' then begin
       DelphiObject.Value := PyObjectAsVariant( value );
       Result := 0;
     end
     else if l_sUpperKey = 'VISIBLE' then begin
       DelphiObject.Visible := PyObjectAsVariant( value );
       Result := 0;
     end
     else
       Result := inherited SetAttrO(key, value);
   except
     on E : Exception do begin
       RaiseDBError( E );
       Result := -1;
     end;
   end;
 end;
end;

procedure TPyDBField.SetDelphiObject(const Value: TField);
begin
 inherited DelphiObject := Value;
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
 if not Assigned(DelphiObject) then begin
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
  Adjust(@Self);  // <- adjust the transmitted self argument
  with GetPythonEngine do begin
    try
      if CheckField then begin
        DelphiObject.Clear;
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
  Adjust(@Self);  // <- adjust the transmitted self argument
  with GetPythonEngine do begin
    try
      if CheckField then begin
        DelphiObject.FocusControl;
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
  Adjust(@Self);   // <- adjust the transmitted self argument
  with GetPythonEngine do begin
    try
      if CheckField then begin
        if PyArg_ParseTuple( args, 's:TField.IsValidChar',@s ) <> 0 then begin
          str := s;
          if Length(str) > 0 then
            c := Char(str[1])
          else
            c := #0;
          Result := VariantAsPyObject( DelphiObject.IsValidChar( c ) )
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
  Adjust(@Self);  // <- adjust the transmitted self argument
  with GetPythonEngine do begin
    try
      if CheckField then begin
        DelphiObject.RefreshLookupList;
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

// ----------------------- TDSRowsAccess ---------------------------------------

class function TDSRowsAccess.ExpectedContainerClass: TClass;
begin
  Result := TFDDataset;
end;

function TDSRowsAccess.GetContainer: TFDDataset;
begin
  Result := TFDDataset(inherited Container);
end;

function TDSRowsAccess.GetItem(aIndex: Integer): PPyObject;
var
  i : Integer;
  l_oDataset: TFDDataset;
begin
  Result := nil;
  l_oDataset := GetContainer;
  if (aIndex >= 0) and (aIndex < l_oDataset.RecordCount) then begin
    with GetPythonEngine do begin
      try
        l_oDataset.RecNo := AIndex + 1;
        Result   := PyTuple_New(l_oDataset.FieldCount);
        for i := 0 to l_oDataset.FieldCount - 1 do begin
          with l_oDataset.Fields[i] do begin
            PyTuple_SetItem( Result, i,  VariantAsPyObject( SqlTimeToVarDate(Value) ) );
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
  l_oDataset := GetContainer;
  Result     := l_oDataset.RecordCount;
end;


class function TDSRowsAccess.Name: String;
begin
  Result := 'DSRows';
end;


// ----------------------- TPyDBDataset ------------------------------------

function  TPyDBDataset.GetDelphiObject: TFDDataset;
begin
  Result := TFDDataset(inherited DelphiObject);
end;

procedure TPyDBDataset.SetDelphiObject(const Value: TFDDataset);
begin
  inherited DelphiObject := Value;
end;


class procedure TPyDBDataset.SetupType(PythonType: TPythonType);
begin
  inherited;
  PythonType.GenerateCreateFunction := True;
  PythonType.Prefix := 'T';
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
   Adjust(@Self);
   Result := GetPythonEngine.PyInt_FromLong(DelphiObject.RecordCount);
end;

function TPyDBDataset.Get_Rows(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := Self.PyDelphiWrapper.DefaultContainerType.CreateInstance;
  with PythonToDelphi(Result) as TPyDelphiContainer do
    Setup(Self.PyDelphiWrapper, TDSRowsAccess.Create(Self.PyDelphiWrapper, Self.DelphiObject));
end;

class procedure TPyDBDataset.RegisterGetSets( PythonType : TPythonType );
begin
 inherited;
 PythonType.AddGetSet(PAnsiChar('RowsCount'), @TPyDBDataset.Get_RowsCount, nil,
       PAnsiChar('Returns the count of contained dataset rows'), nil);
 PythonType.AddGetSet(PAnsiChar('Rows'), @TPyDBDataset.Get_Rows, nil,
       PAnsiChar('Returns an iterator over contained dataset rows'), nil);
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

function  TPyDBDataset.GetAttrO(key: PPyObject) : PPyObject;
var
 l_sUpperKey: String;
 l_oDataset:  TFDDataset;
begin
  Result := nil;
  with GetPythonEngine do begin
    try
      l_oDataset  := DelphiObject;
      l_sUpperKey := UpperCase(PyString_AsDelphiString(Key));
      if l_sUpperKey = 'BOF' then
        Result := VariantAsPyObject( l_oDataset.BOF )
      else if l_sUpperKey = 'CANMODIFY' then
        Result := VariantAsPyObject( l_oDataset.CanModify )
      else if l_sUpperKey = 'EOF' then
        Result := VariantAsPyObject( l_oDataset.EOF )
      else if l_sUpperKey = 'FIELDCOUNT'  then
        Result := VariantAsPyObject( l_oDataset.FieldCount )
      else if l_sUpperKey = 'RECNO'  then
        Result := VariantAsPyObject( l_oDataset.RecNo )
      else
        Result := inherited GetAttrO(key);
    except
      on E : Exception do begin
        RaiseDBError( E );
        Result := Nil;
      end;
    end;
  end;
end;

function  TPyDBDataset.SetAttrO(key, value: PPyObject) : Integer;
var
  l_sUpperKey: String;
  l_oDataset:  TFDDataset;
begin
  Result := -1;
  with GetPythonEngine do begin
    try
      l_oDataset  := DelphiObject;
      l_sUpperKey := UpperCase(PyString_AsDelphiString(Key));
      if l_sUpperKey = 'FILTER' then begin
        l_oDataset.Filter := PyObjectAsVariant( value );
        Result := 0;
      end
      else if l_sUpperKey = 'FILTERED' then begin
        l_oDataset.Filtered := PyObjectAsVariant( value );
        Result := 0;
      end
      else if l_sUpperKey = 'BOF' then begin
        Result := 0;
      end
      else if l_sUpperKey = 'CanModify' then begin
        Result := 0;
      end
      else if l_sUpperKey = 'EOF' then begin
        Result := 0;
      end
      else if l_sUpperKey = 'RECNO' then begin
        l_oDataset.RecNo := PyObjectAsVariant( value );
        Result := 0;
      end
      else
        Result := inherited SetAttrO(key, value);
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
  l_oDataset: TFDDataset;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    l_oDataset := DelphiObject;
    try
      if PyArg_ParseTuple( args, 'i:DBDataset.Fields',@idx ) <> 0 then begin
        if (idx >= 0) and (idx < l_oDataset.FieldCount) then
          Result := PyDelphiWrapper.Wrap(l_oDataset.Fields[idx])
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
  l_oDataset: TFDDataset;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    l_oDataset := DelphiObject;
    try
      if (PyArg_ParseTuple( args, PAnsiChar('s:DBDataset.FieldByName'),@s ) <> 0) then begin
        l_sAStr := AnsiString(s);
        fld := l_oDataset.FieldByName(String(l_sAStr));
        if Assigned(fld) then
          Result := PyDelphiWrapper.Wrap(fld)
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
  l_oDataset: TFDDataset;
begin
  Adjust(@Self);
  with GetPythonEngine do begin
    l_oDataset := DelphiObject;
    try
      Result   := PyTuple_New(l_oDataset.FieldCount);
      for i := 0 to l_oDataset.FieldCount - 1 do
        with l_oDataset.Fields[i] do
          PyTuple_SetItem(Result, i, PyUnicode_FromWideString(FieldName));
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
  l_oDataset: TFDDataset;
begin
  Result := nil;
  Adjust(@Self);     // <- adjust the transmitted self argument
  with GetPythonEngine do begin
    l_oDataset := DelphiObject;
    try
      Result := PyTuple_New(l_oDataset.FieldCount);
      for i := 0 to l_oDataset.FieldCount - 1 do begin
        PyTuple_SetItem( Result, i,
          VariantAsPyObject( SqlTimeToVarDate(l_oDataset.Fields[i].AsVariant )) );
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
  l_oDataset: TFDDataset;
  obj : PPyObject;
  _fieldName : PPyObject;
begin
   Adjust(@Self);     // <- adjust the transmitted self argument
  with GetPythonEngine do begin
    l_oDataset := DelphiObject;
    try
      Result := PyDict_New;
      for i := 0 to l_oDataset.FieldCount - 1 do
        with l_oDataset.Fields[i] do begin
          obj := VariantAsPyObject( SqlTimeToVarDate(AsVariant) );
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
  Adjust(@Self);
  with GetPythonEngine do begin
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
  Adjust(@Self);
  with GetPythonEngine do begin
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
  Adjust(@Self);
  with GetPythonEngine do begin
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
  Adjust(@Self);
  with GetPythonEngine do begin
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
  Adjust(@Self);
  with GetPythonEngine do begin
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
  Adjust(@Self);
  with GetPythonEngine do begin
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
  Adjust(@Self);
  with GetPythonEngine do begin
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
  Adjust(@Self);
  with GetPythonEngine do begin
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
  Adjust(@Self);
  with GetPythonEngine do begin
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

destructor TPyDBTable.Destroy;
begin
  if Assigned(DelphiObject) then
    DelphiObject.Active := False;
  inherited;
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
              AnsiStrin g('FDTable.SetRangeStart() -> None') );
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

function  TPyDBTable.GetAttrO(key: PPyObject) : PPyObject;
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
      l_sUpperKey := UpperCase(PyString_AsDelphiString(Key));
      if l_sUpperKey = 'CONNECTIONDEFNAME' then begin
        l_oConn := DelphiObject.Connection;
        l_sConnectionDefName := DelphiObject.ConnectionName;
        if Assigned(l_oConn) then
          l_sConnectionDefName := l_oConn.ConnectionName;
        Result := VariantAsPyObject( l_sConnectionDefName )
      end
      else if l_sUpperKey = 'DATABASENAME' then begin
        l_oConn := DelphiObject.Connection;
        if Assigned(l_oConn) then
          l_sDatabaseName := l_oConn.Params.Database
        else
          l_sDatabaseName := '';
        Result := VariantAsPyObject( l_sDatabaseName )
      end
      else if l_sUpperKey = 'TABLENAME'then
        Result := VariantAsPyObject( l_oTable.TableName )
      else if l_sUpperKey = 'ACTIVE' then
        Result := VariantAsPyObject( l_oTable.Active )
      else if l_sUpperKey = 'FILTER' then
        Result := VariantAsPyObject( l_oTable.Filter )
      else if l_sUpperKey = 'FILTERED' then
        Result := VariantAsPyObject( l_oTable.Filtered )
      else if l_sUpperKey = 'STATE' then
        Result := VariantAsPyObject( Integer(l_oTable.State) )
      else if l_sUpperKey = 'MODIFIED' then
        Result := VariantAsPyObject( l_oTable.Modified )
      else if l_sUpperKey = 'ISRANGED' then
        // Ermöglicht das Ermitteln des aktuellen Bereichsfilterungsmodus.
        Result := VariantAsPyObject( l_oTable.IsRanged )
      else if l_sUpperKey = 'KEYEXCLUSIVE' then
        // Ermittelt oder setzt die Einbeziehung der niedrigsten und höchsten Werte in einen gefilterten Bereich.
        Result := VariantAsPyObject( l_oTable.KeyExclusive )
      else if l_sUpperKey = 'KEYFIELDCOUNT' then
        // Ermittelt oder setzt die Anzahl der in der Bereichsfilterung zu verwendenden Indexfelder
        Result := VariantAsPyObject( l_oTable.KeyFieldCount )
      else if l_sUpperKey = 'INDEXNAME' then
        Result := VariantAsPyObject( l_oTable.IndexName )
     else
       Result := inherited GetAttrO(key);
    except
      on E : Exception do begin
        RaiseDBError( E );
        Result := nil;
      end;
    end;
  end;
end;

function  TPyDBTable.SetAttrO(key, value: PPyObject) : Integer;
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
      l_sUpperKey := UpperCase(PyString_AsDelphiString(Key));
      if l_sUpperKey = 'CONNECTIONDEFNAME' then begin
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
      else if l_sUpperKey = 'DATABASENAME' then begin
        if CheckActiveDBTable(False) then begin
          l_sDatabaseName := PyObjectAsVariant( value );
          l_oConn := DelphiObject.Connection;
          if Assigned(l_oConn) then begin
            l_oConn.Params.Database := l_sDatabaseName;
          end;
          Result := 0;
        end;
      end
      else if l_sUpperKey = 'TABLENAME' then begin
        if CheckActiveDBTable(False) then begin
          l_oTable.TableName := PyObjectAsVariant( value );
          Result := 0;
        end;
      end
      else if l_sUpperKey = 'ACTIVE' then begin
        l_oTable.Active := PyObjectAsVariant( value );
        Result := 0;
      end
      else if l_sUpperKey = 'FILTER' then begin
        l_oTable.Filter := PyObjectAsVariant( value );
        Result := 0;
      end
      else if l_sUpperKey = 'FILTERED' then begin
        l_oTable.Filtered := PyObjectAsVariant( value );
        Result := 0;
      end
      else if l_sUpperKey = 'STATE' then begin
        Result := 0;
      end
      else if l_sUpperKey = 'MODIFIED' then begin
        Result := 0;
      end
      else if l_sUpperKey = 'ISRANGED' then begin
        Result := 0;
      end
      else if l_sUpperKey = 'KEYEXCLUSIVE' then begin
        // Ermittelt oder setzt die Einbeziehung der niedrigsten und höchsten Werte in einen gefilterten Bereich.
        l_oTable.KeyExclusive := PyObjectAsVariant( value );
        Result := 0;
      end
      else if l_sUpperKey = 'KEYFIELDCOUNT' then begin
        // Ermittelt oder setzt die Anzahl der in der Bereichsfilterung zu verwendenden Indexfelder
        l_oTable.KeyFieldCount := PyObjectAsVariant( value );
        Result := 0;
      end
      else if l_sUpperKey = 'INDEXNAME' then begin
        // Ermittelt oder setzt die Anzahl der in der Bereichsfilterung zu verwendenden Indexfelder
        l_oTable.IndexName := PyObjectAsVariant( value );
        Result := 0;
      end
      else
        Result := inherited SetAttrO(key, value);
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
  Adjust(@Self);   // <- Adjust the transmitted self argument
  with GetPythonEngine do begin
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
  Adjust(@Self);
  with GetPythonEngine do begin
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
  Adjust(@Self);
  with GetPythonEngine do begin
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
  Adjust(@Self);
  with GetPythonEngine do begin
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
  Adjust(@Self);
  with GetPythonEngine do begin
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
  Adjust(@Self);
  with GetPythonEngine do begin
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
  Adjust(@Self);
  with GetPythonEngine do begin
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
  Adjust(@Self);
  with GetPythonEngine do begin
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
  Adjust(@Self);
  with GetPythonEngine do begin
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
  Adjust(@Self);
  with GetPythonEngine do begin
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
  Adjust(@Self);
  with GetPythonEngine do begin
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
  Adjust(@Self);
  with GetPythonEngine do begin
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
  Adjust(@Self);
  with GetPythonEngine do begin
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
  Adjust(@Self);
  with GetPythonEngine do begin
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
  Adjust(@Self);
  with GetPythonEngine do begin
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
  L := nil;
  Adjust(@Self);
  with GetPythonEngine do begin
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

destructor TPyDBQuery.Destroy;
begin
  if Assigned(DelphiObject) then
    DelphiObject.Active := False;
  inherited;
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

function  TPyDBQuery.GetAttrO(key: PPyObject) : PPyObject;
var
  l_sUpperKey: String;
  l_sConnectionDefName, l_sDatabaseName: String;
  l_oConn: TFDCustomConnection;
  l_oQuery: TFDQuery;
begin
  Result := nil;
  with GetPythonEngine do begin
    l_oConn := Nil;
    try
      l_oQuery    := DelphiObject;
      l_sUpperKey := UpperCase(PyString_AsDelphiString(Key));
      if l_sUpperKey = 'CONNECTIONDEFNAME' then begin
        l_oConn := DelphiObject.Connection;
        l_sConnectionDefName := DelphiObject.ConnectionName;
        if Assigned(l_oConn) then
          l_sConnectionDefName := l_oConn.ConnectionName;
        Result := VariantAsPyObject( l_sConnectionDefName )
      end
      else if l_sUpperKey = 'DATABASENAME' then begin
        l_oConn := DelphiObject.Connection;
        if Assigned(l_oConn) then
          l_sDatabaseName := l_oConn.Params.Database
        else
          l_sDatabaseName := '';
        Result := VariantAsPyObject( l_sDatabaseName )
      end
      else if l_sUpperKey = 'PARAMCOUNT' then
        Result := VariantAsPyObject( l_oQuery.ParamCount )
      else if l_sUpperKey = 'PREPARED' then
        Result := VariantAsPyObject( l_oQuery.Prepared )
      else
        Result := inherited GetAttrO(key);
    except
      on E : Exception do begin
        RaiseDBError( E );
        Result := nil;
      end;
    end;
  end;
end;

function  TPyDBQuery.SetAttrO(key, value: PPyObject) : Integer;
var
  i: Integer;
  l_sUpperKey: String;
  l_sName, l_sConnectionDefName, l_sDatabaseName: String;
  l_oConn: TFDCustomConnection;
  l_oQuery: TFDQuery;
begin
  Result := -1;
  with GetPythonEngine do begin
    l_oConn := Nil;
    try
      l_oQuery    := DelphiObject;
      l_sUpperKey := UpperCase(PyString_AsDelphiString(Key));
      if l_sUpperKey = 'CONNECTIONDEFNAME' then begin
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
      else if l_sUpperKey = 'DATABASENAME' then begin
        if CheckActiveDBQuery(False) then begin
          l_sDatabaseName := PyObjectAsVariant( value );
          l_oConn := DelphiObject.Connection;
          if Assigned(l_oConn) then begin
            l_oConn.Params.Database := l_sDatabaseName;
          end;
          Result := 0;
        end;
      end
      else if l_sUpperKey = 'PARAMCOUNT' then begin
        Result := 0;
      end
      else if l_sUpperKey = 'PREPARED' then begin
        l_oQuery.Prepared := PyObjectAsVariant( value );
        Result := 0;
      end
      else
        Result := inherited SetAttrO(key, value);
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
  Adjust(@Self);   // <- Adjust the transmitted self argument
  with GetPythonEngine do begin
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
  Adjust(@Self);
  with GetPythonEngine do begin
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
  Adjust(@Self);
  with GetPythonEngine do begin
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
  Adjust(@Self);
  with GetPythonEngine do begin
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
  Adjust(@Self);
  with GetPythonEngine do begin
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

{ TPyDSRowsRegistration }

procedure TPyFireDACRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  // Values for type TDatasetState
  APyDelphiWrapper.DefineVar('dsInactive', 0);
  APyDelphiWrapper.DefineVar('dsBrowse', 1);
  APyDelphiWrapper.DefineVar('dsEdit', 2);
  APyDelphiWrapper.DefineVar('dsInsert', 3);
  APyDelphiWrapper.DefineVar('dsSetKey', 4);
  APyDelphiWrapper.DefineVar('dsCalcFields', 5);
  APyDelphiWrapper.DefineVar('dsFilter', 6);
  APyDelphiWrapper.DefineVar('dsNewValue', 7);
  APyDelphiWrapper.DefineVar('dsOldValue', 8);
  APyDelphiWrapper.DefineVar('dsCurValue', 9);
  // Values for type TFieldType
  APyDelphiWrapper.DefineVar('ftUnknown', 0);
  APyDelphiWrapper.DefineVar('ftString', 1);
  APyDelphiWrapper.DefineVar('ftSmallint', 2);
  APyDelphiWrapper.DefineVar('ftInteger', 3);
  APyDelphiWrapper.DefineVar('ftWord', 4);
  APyDelphiWrapper.DefineVar('ftBoolean', 5);
  APyDelphiWrapper.DefineVar('ftFloat', 6);
  APyDelphiWrapper.DefineVar('ftCurrency', 7);
  APyDelphiWrapper.DefineVar('ftBCD', 8);
  APyDelphiWrapper.DefineVar('ftDate', 9);
  APyDelphiWrapper.DefineVar('ftTime', 10);
  APyDelphiWrapper.DefineVar('ftDateTime', 11);
  APyDelphiWrapper.DefineVar('ftBytes', 12);
  APyDelphiWrapper.DefineVar('ftVarBytes', 13);
  APyDelphiWrapper.DefineVar('ftAutoInc', 14);
  APyDelphiWrapper.DefineVar('ftBlob', 15);
  APyDelphiWrapper.DefineVar('ftMemo', 16);
  APyDelphiWrapper.DefineVar('ftGraphic', 17);
  APyDelphiWrapper.DefineVar('ftFmtMemo', 18);
  APyDelphiWrapper.DefineVar('ftParadoxOle', 19);
  APyDelphiWrapper.DefineVar('ftDBaseOle', 20);
  APyDelphiWrapper.DefineVar('ftTypedBinary', 21);
  APyDelphiWrapper.DefineVar('ftCursor', 22);
  // Values for type TFieldKind
  APyDelphiWrapper.DefineVar('fkData', 0);
  APyDelphiWrapper.DefineVar('fkCalculated', 1);
  APyDelphiWrapper.DefineVar('fkLookup', 2);
  APyDelphiWrapper.DefineVar('fkInternalCalc', 3);
  // Values for type TLocateOption
  APyDelphiWrapper.DefineVar('loCaseInsensitive', 0);
  APyDelphiWrapper.DefineVar('loPartialKey', 1);
  // Values for type TLockType
  APyDelphiWrapper.DefineVar('ltReadLock', 0);
  APyDelphiWrapper.DefineVar('ltWriteLock', 1);
  // Values for type TIndexOptions
  APyDelphiWrapper.DefineVar('ixPrimary', 0);
  APyDelphiWrapper.DefineVar('ixUnique', 1);
  APyDelphiWrapper.DefineVar('ixDescending', 2);
  APyDelphiWrapper.DefineVar('ixCaseInsensitive', 3);
  APyDelphiWrapper.DefineVar('ixExpression', 4);
  // Values for type TDataAction
  APyDelphiWrapper.DefineVar('daFail', 0);
  APyDelphiWrapper.DefineVar('daAbort', 1);
  APyDelphiWrapper.DefineVar('daRetry', 2);
  // Values for type TUpdateKind
  APyDelphiWrapper.DefineVar('ukModify', 0);
  APyDelphiWrapper.DefineVar('ukInsert', 1);
  APyDelphiWrapper.DefineVar('ukDelete', 2);
  // Values for type TUpdateAction
  APyDelphiWrapper.DefineVar('uaFail', 0);
  APyDelphiWrapper.DefineVar('uaAbort', 1);
  APyDelphiWrapper.DefineVar('uaSkip', 2);
  APyDelphiWrapper.DefineVar('uaRetry', 3);
  APyDelphiWrapper.DefineVar('uaApplied', 4);
end;

function TPyFireDACRegistration.Name: String;
begin
  Result := 'FireDac';
end;

procedure TPyFireDACRegistration.RegisterWrappers(
  aPyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDBDataset);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDBTable);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDBQuery);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDBField);
end;

function SqlTimeToVarDate(V : Variant) : Variant;
begin
  if VarIsSQLTimeStamp(V) or VarIsSQLTimeStampOffset(V) then
    VarCast(Result, V, varDate)
  else
    Result := V;
end;


initialization
begin
  RegisteredUnits.Add( TPyFireDACRegistration.Create );
end;

end.
