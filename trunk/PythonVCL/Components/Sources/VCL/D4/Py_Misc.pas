unit Py_Misc;

interface

uses Classes, Forms, Controls, SysUtils, PythonEngine, PyDelphiAssoc, TypInfo;

const
  kAssocObjectName = '__assoc__';

  procedure CheckError;
  function  GetInstanceSymbol( obj : PPyObject; const symbol : String ) : PPyObject;
  function  GetObjectOf( obj : PPyObject ) : TObject;
  function  DefInterfaceObject( obj : TObject; inst : PPyObject; auto_free : Boolean ) : TDelphiAssoc;
  procedure ClearInterface( delphi_assoc : TDelphiAssoc );
  procedure ClearRefs( ref, current : PPyObject );
  function  SetAttrString( inst : PPyObject; const attr : String; val : PPyObject ) : Integer;
  function  GetInstanceDict( instance : PPyObject ) : PPyObject;
  function  GetInstanceClass( instance : PPyObject ) : PPyObject;
  function  AddNewMember( Inst : PPyObject; const Attr : String; Val : PPyObject) : PPyObject;
  function  GetPropInteger( obj : TObject; const PropName : String ) : Integer;
  procedure SetPropInteger( obj : TObject; const PropName : String; Value : Integer );
  function  GetPythonObject( obj : TObject; const AUnit, AClass : String ) : PPyObject;
  procedure GetProps( obj : TObject; strings : TStrings );
  function  GetPropList( obj : TObject ) : PPyObject;
  function  GetPropertyAsPyObject( obj : TObject; const PropName : String ) : PPyObject;
  function  SetPropertyFromPyObject( obj : TObject; const PropName : String; value : PPyObject ) : Boolean;
  function  ObjectRepr( obj : PPyObject ) : String;
  procedure UnknownProperty( inst : PPyObject; const PropName : String );
  procedure ErrorNoObject( inst : PPyObject );
  procedure ErrorWrongExpectedType( inst : PPyObject; const typeName : String );
  procedure ErrorWrongPropertyType( inst : PPyObject; obj : TObject; const propName, propType : String );
  procedure ErrorWrongSetPropertyType( inst : PPyObject; const propName, propType : String );
  procedure ErrorTypeNotHandled( inst : PPyObject; const propName, propType : String );
  procedure ErrorPropReadOnly( inst : PPyObject; const propName : String );
  procedure ErrorBadEventType( inst : PPyObject; const propName : String );
  function  SetToList( data : Pointer; size : Integer ) : PPyObject;
  procedure ListToSet( List : PPyObject; data : Pointer; size : Integer );
  function  CheckMethod( obj : PPyObject ) : Boolean;
  function  FindEventInInstance( instance : PPyObject; const event : String ) : PPyObject;
  function  ExecuteEvent( const event_name : String; assoc : TDelphiAssoc; args : TList; var argsList : PPyObject ) : PPyObject;
  function  PyObjectAsChar( obj : PPyObject ) : Char;

implementation

procedure CheckError;
begin
  with GetPythonEngine do
    if PyErr_Occurred <> nil then
      begin
        PyErr_Print;
        RaiseError;
      end;
end;

function  GetInstanceSymbol( obj : PPyObject; const symbol : String ) : PPyObject;
var
  i : PPyInstanceObject;
begin
  with GetPythonEngine do
    begin
      Result := nil;
      if Assigned(obj) and PyInstance_Check(obj) then
        begin
          i := PPyInstanceObject(obj);
          Result := PyDict_GetItemString( i^.in_dict, PChar(symbol) );
        end
      else
        PyErr_SetString (PyExc_TypeError^, PChar('instance object expected'));
    end;
end;

function GetObjectOf( obj : PPyObject ) : TObject;
var
  symb : PPyObject;
begin
  with GetPythonEngine do
    begin
      Result := nil;
      if obj = Py_None then
        Exit;
      symb := GetInstanceSymbol( obj, kAssocObjectName );
      if Assigned(symb) then
        begin
          if PyDelphiAssoc_Check(symb) then
            begin
              Result := (PythonToDelphi(symb) as TDelphiAssoc).da_delphi_obj;
            end
          else
            PyErr_SetString (PyExc_AttributeError^, PChar('attribute "'+kAssocObjectName+'" must be a DelphiObject'));
        end
      else
        PyErr_SetString (PyExc_AttributeError^, PChar('Could not find attribute "'+kAssocObjectName+'" in the instance'));
    end;
end;

function  DefInterfaceObject( obj : TObject; inst : PPyObject; auto_free : Boolean ) : TDelphiAssoc;
begin
  with GetPythonEngine do
    begin
      Result := nil;
      if Assigned(inst) and PyInstance_Check(inst) then
        begin
          Result := CreateDelphiAssocWith(obj,inst,auto_free);
          if Assigned( Result ) then
            begin
              SetAttrString(inst, kAssocObjectName, Result.GetSelf );
              Py_DECREF( Result.GetSelf );
            end;
        end
      else
        PyErr_SetString (PyExc_TypeError^, PChar('instance object needed for delphi component creation'));
      SetPropInteger( obj, kAssocObjectName, Integer(Result) );
{      if GetPropInteger( obj, kAssocObjectName ) = 0 then
        SetPropInteger( obj, 'Tag', Integer(Result) );}
    end;
end;

procedure ClearInterface( delphi_assoc : TDelphiAssoc );
var
  tmp : PPyObject;
  auto_free : Boolean;
begin
  if Assigned( delphi_assoc ) and PythonOK then
    with GetPythonEngine, delphi_assoc do
    begin
      auto_free := da_auto_free;
      ClearDelphiObj;
      tmp := da_Python_Inst;
      ClearPythonInst;
      ClearRefs( tmp, tmp );
      if auto_free then
        Py_XDecRef( tmp );
    end;
end;

procedure ClearRefs( ref, current : PPyObject );

  function IsOurMethod( ref, current : PPyObject ) : Boolean;
  var
    meth  : PPyMethodObject;
  begin
    Result := False;
    with GetPythonEngine do
      if Assigned(current) and PyMethod_Check(current) then
        begin
          // if we have a ref on a method in our main object (instance)
          meth := PPyMethodObject(current);
          if meth^.im_self = ref then
            Result := True; // then we want to delete it
        end
  end;

  procedure CheckMapping( ref, mapping : PPyObject );
  var
    keys  : PPyObject;
    key   : PPyObject;
    val   : PPyObject;
    i     : Integer;
    clear : Boolean;
  begin
    with GetPythonEngine do
      begin
        // Get the keys of the dict
        keys := PyObject_CallMethod(mapping, 'keys', nil);
        CheckError;
        if not Assigned(keys) then
          Exit;
        if PySequence_Check(keys) = 0 then
          begin
            Py_DecRef(keys);
            Exit;
          end;
        try
          // For each key of the dict
          for i := 0 to PyObject_Length(keys) - 1 do
            begin
              clear := False;
              // Get Key
              key := PySequence_GetItem(keys, i);
              CheckError;
              try
                // Get Value of Key
                val := PyObject_GetItem(mapping, key);
                CheckError;
                try
                  // If an object stores a ref of our main object (instance)
                  if val = ref then
                    clear := True // then we want to delete it to avoid circular references
                  else if IsOurMethod( ref, val ) then
                    clear := True // then we want to delete it
                  {else if PyDelphiAssoc_Check(val) then
                    clear := True}
                  // if we have a ref on an external object
                  else
                    ClearRefs(ref, val); // we want to call us recursively, in order to clear all refs
                  // if deletion is required
                  if clear then // then do it
                    PyObject_SetItem(mapping, key, Py_None );
                finally
                  Py_XDECREF(val);
                end;
              finally
                Py_XDecRef(key);
              end;
            end;
        finally
          Py_DECREF(keys);
        end;
      end;
  end;

  procedure CheckSequence( ref, seq : PPyObject );
  var
    i : Integer;
    val : PPyObject;
  begin
    with GetPythonEngine do
      begin
        for i := 0 to PyObject_Length(seq) - 1 do
          begin
            val := PySequence_GetItem(seq, i);
            try
              if (val = ref) or IsOurMethod( ref, val) then
                begin
                  //Py_XIncRef( Py_None );
                  PySequence_SetItem(seq, i, Py_None)
                end
              else
                ClearRefs(ref, val);
            finally
              Py_XDecRef(val);
            end;
          end;
      end;
  end;

  procedure CheckInstance( ref, inst : PPyObject );
  var
    dict  : PPyObject;
  begin
    // Get the dict of instance symbols
    With GetPythonEngine do
      begin
        dict := GetInstanceDict(inst);
        if not Assigned(dict) then
          Exit;
        CheckMapping( ref, dict );
      end;
  end;

begin
  if not Assigned(current) or not Assigned(ref) then
     Exit;
  With GetPythonEngine do
    begin
      if PyInstance_Check(current) then
         CheckInstance(ref, current)
      else if PyMapping_Check(current) <> 0 then
         CheckMapping(ref, current)
      else if PySequence_Check(current) <> 0 then
         CheckSequence(ref, current);
    end;
end;

function SetAttrString( inst : PPyObject; const attr : String; val : PPyObject ) : Integer;
var
  dict : PPyObject;
begin
  with GetPythonEngine do
    begin
      Result := -1;
      dict := GetInstanceDict(inst);
      if Assigned(dict) then
        Result := PyDict_SetItemString( dict, PChar(attr), val );
    end;
end;

function GetInstanceDict( instance : PPyObject ) : PPyObject;
var
  i : PPyInstanceObject;
begin
  with GetPythonEngine do
    begin
      Result := nil;
      if Assigned(instance) and PyInstance_Check(instance) then
        begin
          i := PPyInstanceObject(instance);
          Result := i^.in_dict;
        end
      else
        PyErr_SetString (PyExc_TypeError^, PChar('instance object expected'));
    end;
end;

function  GetInstanceClass( instance : PPyObject ) : PPyObject;
var
  i : PPyInstanceObject;
begin
  with GetPythonEngine do
    begin
      Result := nil;
      if Assigned(instance) and PyInstance_Check(instance) then
        begin
          i := PPyInstanceObject(instance);
          Result := PPyObject(i^.in_class);
        end
      else
        PyErr_SetString (PyExc_TypeError^, PChar('instance object expected'));
    end;
end;

function AddNewMember( Inst : PPyObject; const Attr : String; Val : PPyObject) : PPyObject;
begin
  with GetPythonEngine do
    begin
      if PyErr_Occurred <> nil then
        PyErr_Clear;
      SetAttrString( Inst, PChar(Attr), Val );
      Result := PyInt_FromLong( 0 );
    end;
end;

function GetPropInteger( obj : TObject; const PropName : String ) : Integer;
var
  PropInfo : PPropInfo;
begin
  Result := 0;
  if Assigned(obj) then
    begin
      PropInfo := GetPropInfo( obj.ClassInfo, PropName );
      if Assigned(PropInfo) then
        Result := GetOrdProp( obj, PropInfo );
    end;
end;

procedure SetPropInteger( obj : TObject; const PropName : String; Value : Integer );
var
  PropInfo : PPropInfo;
begin
  if Assigned(obj) then
    begin
      PropInfo := GetPropInfo( obj.ClassInfo, PropName );
      if Assigned(PropInfo) then
        SetOrdProp( obj, PropInfo, Value );
    end;
end;

function GetPythonObject( obj : TObject; const AUnit, AClass : String ) : PPyObject;

  function CreateInst( const AUnit, AClass : String ) : PPyObject;
  var
    m, d, cl : PPyObject;
    //inst : PPyInstanceObject;
  begin
    Result := nil;
    with GetPythonEngine do
      begin
        m := PyImport_ImportModule( PChar(AUnit) );
        if Assigned(m) then
          begin
            d := PyModule_GetDict( m );
            cl := PyDict_GetItemString( d, PChar(AClass) );
            if Assigned(cl) then
              begin
                Result := PyInstance_New( cl, nil, nil );
                DefInterfaceObject( obj, Result, False );
                {inst := PPyInstanceObject( Py_Malloc( sizeof(PyInstanceObject) ) );
                if Assigned(inst) then
                  begin
                    inst^.ob_refcnt := 1;
                    inst^.ob_type := PyInstance_Type;
                    inst^.in_class := PPyClassObject(cl);
                    Py_XINCREF(cl);
                    inst^.in_dict := PyDict_New;
                    Result := PPyObject(inst);
                    DefInterfaceObject( obj, Result, False );
                  end;}
              end;
          end;
      end;
  end;

var
  val : Longint;
  info : PTypeInfo;
  data : PTypeData;
  sUnit, sClass : String;
  tmp : PChar;
begin
  with GetPythonEngine do
    begin
      Result := nil;
      if Assigned(obj) then
        begin
          // we look for a property in the object that's called
          // '__Assoc__' which will transmit us the
          // DelphiAssoc stored in the object
          val := GetPropInteger( obj, kAssocObjectName );
          if val <> 0 then
            begin
              Result := TDelphiAssoc(val).da_python_inst;
              Py_XINCREF( Result );
            end;
          // else we look for a property in the object that's called
          // 'Tag' which will transmit us the
          // DelphiAssoc stored in the object
{          if val = 0 then
            begin
              val := GetPropInteger( obj, 'Tag');
              if val <> 0 then
                begin
                  Result := TDelphiAssoc(val).da_python_inst;
                  Py_XINCREF( Result );
                end
            end;}
          if val = 0 then
            begin
              // Else we create an instance of the matching Python class
              info := PTypeInfo( obj.ClassInfo );
              tmp := PChar(info)+sizeof(TTypeKind);
              tmp := tmp + Ord(tmp^)+1;
              data := PTypeData(tmp);
              sUnit := data^.UnitName;
              sClass := obj.ClassName;
              Result := CreateInst( sUnit, sClass );
            end;
          if not Assigned(Result) then
            Result := CreateInst( AUnit, AClass );
        end;
      // If we could'nt find a DelphiAssoc, we return None
      if not Assigned(Result) then
        Result := ReturnNone;
    end;
end;

procedure GetProps( obj : TObject; strings : TStrings );
var
  I, Count: Integer;
  PropInfo: PPropInfo;
  TempList: PPropList;
begin
  Count := GetTypeData(obj.ClassInfo)^.PropCount;
  if Count > 0 then
  begin
    GetMem(TempList, Count * SizeOf(Pointer));
    try
      GetPropInfos(obj.ClassInfo, TempList);
      for I := 0 to Count - 1 do
      begin
        PropInfo := TempList^[I];
        strings.Add(PropInfo^.Name);
      end;
    finally
      FreeMem(TempList, Count * SizeOf(Pointer));
    end;
  end;
end;

function GetPropList( obj : TObject ) : PPyObject;
var
  L : TStringList;
begin
  with GetPythonEngine do
    begin
      L := TStringList.Create;
      try
        GetProps( obj, L );
        Result := StringsToPyList( L );
      finally
        L.Free;
      end;
    end;
end;

function  GetPropertyAsPyObject( obj : TObject; const PropName : String ) : PPyObject;
var
  PropInfo : PPropInfo;
  //val : Integer;
begin
  with GetPythonEngine do
    begin
      Result := nil;
      if not Assigned(obj) then
        Exit;
      PropInfo := GetPropInfo( obj.ClassInfo, PropName );
      if PropInfo <> nil then
        begin
          with PropInfo.PropType^^, GetTypeData(PropInfo.PropType^)^ do
            begin
              case Kind of
                tkInteger,
                tkChar,
                tkWChar:    Result := PyInt_FromLong(GetOrdProp( obj, PropInfo ));
                tkFloat:    Result := PyFloat_FromDouble(GetFloatProp( obj, PropInfo ));
                tkString,
                tkLString,
                tkWString:  Result := PyString_FromString(PChar(GetStrProp( obj, PropInfo )));
                tkEnumeration:
                  begin
                    //val := GetOrdProp( obj, PropInfo );
                    Result := PyInt_FromLong(GetOrdProp( obj, PropInfo ) );
                    //Result := PyString_FromString(PChar(GetEnumName(PropInfo.PropType^,val)));
                  end;
                tkSet:
                  begin
                    Result := PyInt_FromLong(GetOrdProp( obj, PropInfo ));
                  end;
                else
                  Result := ReturnNone;
              end;
            end;
        end;
    end;
end;

function SetPropertyFromPyObject( obj : TObject; const PropName : String; value : PPyObject ) : Boolean;
var
  PropInfo : PPropInfo;
  s : String;
begin
  with GetPythonEngine do
    begin
      if not Assigned(obj) then
        begin
          Result := False;
          Exit;
        end;
      Result := True;
      PropInfo := GetPropInfo( obj.ClassInfo, PropName );
      if PropInfo <> nil then
        begin
          with PropInfo.PropType^^, GetTypeData(PropInfo.PropType^)^ do
            begin
              case Kind of
                tkInteger,
                tkChar,
                tkWChar:    SetOrdProp( obj, PropInfo, PyObjectAsVariant(value));
                tkFloat:    SetFloatProp( obj, PropInfo, PyObjectAsVariant(value));
                tkString,
                tkLString,
                tkWString:  begin
                  s := PyObjectAsVariant(value);
                  SetStrProp( obj, PropInfo, s);
                end;
                tkEnumeration:
                  begin
                    SetOrdProp( obj, PropInfo, PyObjectAsVariant(value) );
                    //SetOrdProp( obj, PropInfo, GetEnumValue(PropInfo.PropType^, GetStr(value)) );
                  end;
                tkSet:
                  begin
                    SetOrdProp( obj, PropInfo, PyObjectAsVariant(value) );
                  end;
                else
                  Result := False;
              end;
            end;
        end
      else
        Result := False;
    end;
end;

function ObjectRepr( obj : PPyObject ) : String;
var
  inst : PPyInstanceObject;
begin
  with GetPythonEngine do
    begin
      if Assigned(obj) and PyInstance_Check( obj ) then
        begin
          inst := PPyInstanceObject(obj);
          Result := Format('<%s instance at %p>',[PyObjectAsString(inst^.in_class^.cl_name),inst]);
        end
      else
        Result := PyObjectAsString( obj );
    end;
end;

procedure UnknownProperty( inst : PPyObject; const PropName : String );
begin
  with GetPythonEngine do
    PyErr_SetString (PyExc_AttributeError^, PChar(Format('unknown attribute "%s" of object %s',
                     [PropName, ObjectRepr(inst)])));
end;

procedure ErrorNoObject( inst : PPyObject );
begin
  with GetPythonEngine do
    PyErr_SetString (PyExc_StandardError^, PChar(Format('object %s does not contain a delphi object',
                     [ObjectRepr(inst)])));
end;

procedure ErrorWrongExpectedType( inst : PPyObject; const typeName : String );
begin
  with GetPythonEngine do
    PyErr_SetString (PyExc_TypeError^, PChar(Format('object %s must contain a Delphi object of class %s',
                     [ObjectRepr(inst), typeName])));
end;

procedure ErrorWrongPropertyType( inst : PPyObject; obj : TObject; const propName, propType : String );
var
  s : String;
begin
  if Assigned(obj) then
    s := obj.ClassName
  else
    s := '';
  with GetPythonEngine do
    PyErr_SetString (PyExc_TypeError^, PChar(Format('in object %s, the property %s of type %s received an object of type %s',
                     [ObjectRepr(inst), propName, propType, s])));
end;

procedure ErrorWrongSetPropertyType( inst : PPyObject; const propName, propType : String );
begin
  with GetPythonEngine do
    PyErr_SetString (PyExc_TypeError^, PChar(Format('in object %s, the property %s of type %s is a set and must receive a Python list',
                     [ObjectRepr(inst), propName, propType])));
end;

procedure ErrorTypeNotHandled( inst : PPyObject; const propName, propType : String );
begin
  with GetPythonEngine do
    PyErr_SetString (PyExc_TypeError^, PChar(Format('in object %s, type %s of the property %s is not yet handled',
                     [ObjectRepr(inst), propType, propName])));
end;

procedure ErrorPropReadOnly( inst : PPyObject; const propName : String );
begin
  with GetPythonEngine do
    PyErr_SetString (PyExc_TypeError^, PChar(Format('the property %s of the object %s is ReadOnly',
                     [propName, ObjectRepr(inst)])));
end;

procedure ErrorBadEventType( inst : PPyObject; const propName : String );
begin
  with GetPythonEngine do
    PyErr_SetString (PyExc_TypeError^, PChar(Format('in object %s, the property %s must receive an object''s method.',
                     [ObjectRepr(inst), propName])));
end;

function SetToList( data : Pointer; size : Integer ) : PPyObject;

  function GetBit( idx : Integer ) : Boolean;
  var
    tmp : PChar;
  begin
    if idx >= size*8 then
      begin
        Result := False;
        Exit;
      end;
    tmp := PChar(data);
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
  with GetPythonEngine do
    begin
      Result := PyList_New( cpt );
      cpt := 0;
      for i := 0 to size*8-1 do
        if GetBit(i) then
          begin
            PyList_SetItem( Result, cpt, PyInt_FromLong(i) );
            Inc(cpt);
          end;
    end;
end;

procedure ListToSet( List : PPyObject; data : Pointer; size : Integer );

  procedure SetBit( idx : Integer );
  var
    tmp : PChar;
  begin
    if idx >= size*8 then
      Exit;
    tmp := PChar(data);
    tmp := tmp + (idx div 8);
    tmp^ := Chr((Ord(tmp^) or (1 shl (idx mod 8))));
  end;

var
  i : Integer;
begin
  FillChar( PChar(data)^, size, #0 );
  with GetPythonEngine do
    for i := 0 to PyList_Size(list)-1 do
      SetBit( PyObjectAsVariant( PyList_GetItem(list, i) ) );
end;

function  CheckMethod( obj : PPyObject ) : Boolean;
begin
  with GetPythonEngine do
    Result := PyMethod_Check( obj );
end;

function FindEventInInstance( instance : PPyObject; const event : String ) : PPyObject;
begin
  Result := nil;
  with GetPythonEngine do
    if Assigned(instance) and PyInstance_Check(instance) then
      Result := GetInstanceSymbol( instance, event );
end;

function ExecuteEvent( const event_name : String; assoc : TDelphiAssoc; args : TList; var argsList : PPyObject ) : PPyObject;
var
  Instance : PPyObject;
  event : PPyObject;
  i, j, size : Integer;
begin
  argsList := nil;
  Result := nil;
  with GetPythonEngine do
    begin
      if not Assigned(assoc) or not Assigned(args) then
        Exit;
      Instance := assoc.da_python_inst;
      if not Assigned(Instance) then
        Exit;
      // Look for the event
      event := FindEventInInstance( Instance, event_name );
      if not Assigned(event) then
        Exit; // There was no event defined !

      // Build arguments
      // if we don't call a method, we must add a self argument as
      // first argument.
      size := args.Count;
      argsList := PyTuple_New( size );
      if not Assigned(argsList) then
        Exit;
      // add all args
      // Note that they will automatically be cleared
      // because we don't increment their ref count
      j := 0;
      for i := 0 to args.Count-1 do
        begin
          if args.Items[i] <> nil then
            PyTuple_SetItem( argsList, j, PPyObject(args.Items[i]) )
          else
            PyTuple_SetItem( argsList, j, ReturnNone );
          Inc(j);
        end;
      // Execute the event
      Result := PyEval_CallObject(event, argsList);
      try
        CheckError;
      except
        // Clearing objects
        Py_XDECREF( Result );
        Py_XDECREF( argsList );
        raise;
      end;
    end;
end;

function PyObjectAsChar( obj : PPyObject ) : Char;
var
  v : Integer;
  s : PChar;
begin
  Result := #0;
  if not Assigned(obj) then
    Exit;
  with GetPythonEngine do
    begin
      if PyString_Check(obj) and (PyString_Size(obj) > 0) then
        begin
          s := PyString_AsString(obj);
          Result := s^;
        end
      else if PyInt_Check(obj) then
        begin
          v := PyInt_AsLong(obj);
          if v < 256 then
            Result := Chr(v);
        end;
    end;
end;

end.
