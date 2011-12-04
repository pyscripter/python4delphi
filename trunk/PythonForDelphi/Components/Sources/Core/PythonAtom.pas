{**
 Module:  Unit 'PythonAtom'          Copyright (c) 1999                         <BR>
 Author : Olivier Deckmyn (olivier.deckmyn@mail.dotcom.fr)                      <BR>
                                                                                <BR>
 Version: 1.1                                                                   <BR>
                                                                                <BR>
 Look Python-Delpi page at: Deploying P4D.PDF                                   <BR>
************************************************************************        <BR>
  Functionality:                                                                <BR>
                  This unit allows the developper to manipulate any             <BR>
                  python object directly in the Delphi source code !            <BR>
                  This way, the developper can easily access python             <BR>
                  objects (of any kind) without using the GetAttrXXXX           <BR>
                  methods.                                                      <BR>
                  See TPythonAtom documentation for more explanations           <BR>
                  on how to use this new feature.                               <BR>
                                                                                <BR>
************************************************************************        <BR>
  Contributors:                                                                 <BR>
      Morgan Martinet                                                           <BR>
      Gene Chiaramonte (gchiaramonte@yahoo.com)                                 <BR>
      Sigve Tjora (public@tjora.no)                                             <BR>
      Stefan Franke (franke@meso.net)                                           <BR>
************************************************************************        <BR>
 This source code is distributed with no WARRANTY, for no reason or use.        <BR>
 Everyone is allowed to use and change this code free for his own tasks         <BR>
 and projects, as long as this header and its copyright text is intact.         <BR>
 For changed versions of this code, which are public distributed the            <BR>
 following additional conditions have to be fullfilled:                         <BR>
 1) The header has to contain a comment on the change and the author of         <BR>
    it.                                                                         <BR>
 2) A copy of the changed source has to be sent to the above E-Mail             <BR>
    address or my then valid address, if this is possible to the                <BR>
    author.                                                                     <BR>
 The second condition has the target to maintain an up to date central          <BR>
 version of the component. If this condition is not acceptable for              <BR>
 confidential or legal reasons, everyone is free to derive a component          <BR>
 or to generate a diff file to my or other original sources.                    <BR>
************************************************************************        <BR>
}
{---------------------------------------------------------------------------
  $Header: /P4D/PythonForDelphi/Components/Sources/Core/PythonAtom.pas 1     03-04-09 19:24 Morgan $
  --------------------------------------------------------------------------
  $Log: /P4D/PythonForDelphi/Components/Sources/Core/PythonAtom.pas $
 * 
 * 1     03-04-09 19:24 Morgan
 * initial check-in
 ---------------------------------------------------------------------------}

(* Note : the comments "{**" of this units are formated for Time2Help *)
unit PythonAtom deprecated 'consider using VarPyth instead';
{$I definition.inc}

interface

uses
  Classes, ComObj,
  PythonEngine;

type

  {** This class allows the developper to manipulate any python object directly
      in the Delphi source code ! <BR>
      This way, the developper can easily access pythonobjects (of any kind)
      without using the GetAttrXXXX methods. <BR>
      There is one TPythonAtom instance per python object instance to play with.<BR>
      This TPythonAtom instance is a wrapper around the pythonObject provided
      (during the construction, of after). Its most useful property is AsAtom.<BR>
      This property is an OleVariant, pointing to the IDispatch interface of
      the current instance of TPythonAtom(=self). When using an OleVariant, Delphi's
      compiler allows the developper to use any syntax behind. This "should" be
      used to manipulate an Automation server, without having to have its typelib
      at runtime. Methods and properties invocation are converted at runtime into
      IDispatch.Invoke commands. This object intercepts thoose calls to make an
      indirection and use the GetAttr capabilities of Python Objects.<BR>
      Example of typical Use :
      <code>
      var
        pObject : PPyObject;
        myAtom : OleVariant;
        myString : String;
      begin
        pObject := ... a python object ...(using a TPythonDelphiVar.ValueObject for example);
        myAtom := getAtom(pObject); // This is the magic object
        myAtom.myMethod();
        myString := myAtom.myProperty.myProperty2;
        myAtom.myProperty.myProperty3:='Olivier';
        ShowMessage(myAtom.myMethod2('Olivier'));
        ShowMessage(myAtom.myListProperty[3]);
        // Or anything ! Note that type conversion are automatic !
      end;
      </code>
   }
  TPythonAtom=class(TInterfacedObject, IDispatch)
  private
    FPythonObject : PPyObject;
    function GetAsAtom: OleVariant;
  protected
    {** This methods is used to attach a new PythonObject to the atom. This method
        handles the reference counting (even is value=nil).}
    procedure SetPythonObject(const Value: PPyObject); virtual;
    {** If a pythonObject is attached, this method will increment its reference counter}
    procedure PythonAddRef;
    {** If a pythonObject is attached, this method will decrement its reference counter}
    procedure PythonRelease;
  public
    { IDispatch }
    {** not used }
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    {** not used }
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    {** Given a member name, returns its DispID. Here, if a pythonObject is attached,
        this method will return the address of the requested member name string (in fact,
        a python object), casted as an integer.<BR>
        This way, in the next called method(invoke), the dispID will contain the
        correct python Object.<BR>
        If there is no such member on the python object, E_NOINTERFACE COM error
        is raised.<BR>
        Programmer should never call this method directly, it is automatically
        invoked with call on the IDispatch interface.
        }
    function GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    {** See GetIDsOfNames. DispID should contain here the address of the name of
        the requested member (in fact, a python object), casted as an integer.<BR>
        Params are converted to python objects, and used as params when calling
        the member method.
        Depending on the value of Flags(DISPATCH_PROPERTYGET, DISPATCH_PROPERTYPUT,
        DISPATCH_METHOD), a call is made to a member method or a member attribute.<BR>
        The result of the call is set into varResult, if any. }
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
    { TPythonAtom }
      constructor Create(pObject : PPyObject = nil); virtual;
    {** Before being destroyed, PythonObject is set to nil. This way, the python
        reference counter is handled correctly.}
    destructor Destroy; override;
    {** Just calls TInterfacedObject._AddRef}
    procedure AddRef;
    {** Just calls TInterfacedObject._Release}
    procedure Release;
    {** Here one can attach the pythonObject that is to be atomised.}
    property PythonObject : PPyObject read FPythonObject write SetPythonObject;
    {** This property (OleVariant type) returns the IDispatch interface of self.
        This way, a Delphi programmer can use python method and properties INSIDE
        its Delphi Code !}
    property asAtom : OleVariant read GetAsAtom;
  end;

  {** This helper function converts a python object into a python atom. Look at
      the trivial implementation of this function for more informations.} 
  function GetAtom(pObject : PPyObject) : OleVariant; deprecated;

  {$IFDEF DEBUG}
  {** If this variable is set, (and DEBUG directive is defined) the strings is
      used to show debugging informations inside methods and functions of the
      unit.(see DebugMessage in Implementation)}
   var DebugStrings : TStrings = nil;
  {$ENDIF}

implementation

uses
  ActiveX, Windows, SysUtils;


function GetAtom(pObject : PPyObject) : OleVariant;
begin
  Result := TPythonAtom.Create(pObject).asAtom;
end;

procedure WideCharToShortString(P: PWideChar; var S: ShortString);
var
  I: Integer;
  W: WideChar;
begin
  I := 0;
  repeat
    W := P[I];
    if W = #0 then Break;
    if W >= #256 then W := #0;
    Inc(I);
    S[I] := Char(W);
  until I = 255;
  S[0] := Char(I);
end;


{$IFDEF DEBUG}
procedure DebugMessage(s : String);
begin
  if Assigned(DebugStrings) then DebugStrings.Add('[DEBUG] '+s);
end;

procedure DebugMessageFmt(s : String; args : Array of Const);
begin
  if Assigned(DebugStrings) then DebugStrings.Add(Format('[DEBUG] '+s,args));
end;
{$ENDIF}

{ TPythonAtom }
constructor TPythonAtom.Create(pObject: PPyObject);
begin
  inherited Create;
  PythonObject := pObject;
  {$IFDEF DEBUG}DebugMessage('TPythonAtom.Create');{$ENDIF}
end;

destructor TPythonAtom.Destroy;
begin
  {$IFDEF DEBUG}DebugMessage('TPythonAtom.Destroy');{$ENDIF}
  if PythonOK then PythonObject := nil; //We clean only if PythonEngine is still there !
  inherited;
end;

function TPythonAtom.GetAsAtom: OleVariant;
begin
  {$IFDEF DEBUG}DebugMessage('TPythonAtom.getAsAtom');{$ENDIF}
  result := self as IDispatch;
end;

function TPythonAtom.GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
type
  PNamesArray = ^TNamesArray;
  TNamesArray = array[0..0] of PWideChar;
var
  lName : ShortString;
  lLongName : String;
  lNames : PNamesArray;
  lDispIds : PDispIDList;
  lObject : PPyObject;
begin
  lNames := Names;
  lDispIds:=DispIDs;
  WideCharToShortString(lNames[0], lName);
  lLongName := lName;

  {$IFDEF DEBUG}DebugMessageFmt('TPythonAtom.GetIDsOfNames(IID=%s, NameCount=%d, LocaleID=%d, Names=%s)',[GUIDToString(IID), NameCount, LocaleID, lLongName]);{$ENDIF}

  if CompareText(lLongName,'__asAtom__')=0
  then
    begin
      lDispIds[0]:=-1;
      result:=S_OK;
    end
  else if CompareText(lLongName,'__asPPyObject__')=0
  then
    begin
      lDispIds[0]:=-2;
      result:=S_OK;
    end
  else
    begin
      if GetPythonEngine.PyObject_HasAttrString(FPythonObject, pChar(lLongName))=0
      then
        begin
          result := E_NOINTERFACE;
        end
      else
        begin
          lObject := GetPythonEngine.PyString_FromString(PChar(lLongName));
          lDispIds[0]:= Integer(lObject);  // Dirty storage !
          result := S_OK;
        end;
    end;
end;

function TPythonAtom.GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult;
begin
  {$IFDEF DEBUG}DebugMessage('TPythonAtom.GetTypeInfo(Index=%d, LocaleID=%d)');{$ENDIF}
  result := E_NOTIMPL;
end;

function TPythonAtom.GetTypeInfoCount(out Count: Integer): HResult;
begin
  {$IFDEF DEBUG}DebugMessage('TPythonAtom.GetTypeInfoCount');{$ENDIF}
  result := E_NOTIMPL;
end;

function TPythonAtom.Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
  Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult;

  function IsBasicType( obj : PPyObject ) : Boolean;
  begin
    with GetPythonEngine do
      Result := PyInt_Check(obj) or
                PyFloat_Check(obj) or
                PyString_Check(obj) or
                PyList_Check(obj) or
                PyTuple_Check(obj) or
                PyDate_Check(obj);
  end;

  function IsCallableType( obj : PPyObject ) : Boolean;
  begin
    with GetPythonEngine do
      Result := PyFunction_Check(obj) or
                PyMethod_Check(obj);
  end;

var
  lObject : PPyObject;
  lParams : TDispParams;
  i, ii : integer;
  lParamValue : PPyObject;
  lPyArg : PPyObject;
  lPyResult : PPyObject;
  lVarResult : PVariant;
  lParam : PVariant;
  myVariant: Variant;
  {$IFDEF DEBUG}lFlags : String;{$ENDIF}
begin
  with GetPythonEngine do
    begin
      result := S_OK;
      {$IFDEF DEBUG}
      lFlags := '';
      if (Flags and DISPATCH_METHOD)>0 then lFlags:=lFlags+'DISPATCH_METHOD ';
      if (Flags and DISPATCH_PROPERTYGET)>0 then lFlags:=lFlags+'DISPATCH_PROPERTYGET ';
      if (Flags and DISPATCH_PROPERTYPUT)>0 then lFlags:=lFlags+'DISPATCH_PROPERTYPUT ';
      if (Flags and DISPATCH_PROPERTYPUTREF)>0 then lFlags:=lFlags+'DISPATCH_PROPERTYPUTREF ';
      {$ENDIF}

      lObject := PPyObject(DispID); // Dirty storage !
      lParams := TDispParams(Params);
      lVarResult:= PVariant(varResult);
      if (lParams.cArgs=1) and (lParams.rgvarg^[0].vt=VT_ERROR) then lParams.cArgs:=0; // This is the case when invoking method with ()
      {$IFDEF DEBUG}DebugMessageFmt('TPythonAtom.Invoke(DispID=%d, IID=%s, LocaleID=%d, Flags=%s, argCount=%d, VarResult=%d)',[DispID, GuidToString(IID), LocaleID, lFlags, lParams.cArgs, INteger(VarREsult)]);{$ENDIF}
      if ((Flags and DISPATCH_METHOD)>0) or ((Flags and DISPATCH_PROPERTYGET)>0) // If we have a method invocation or a property get
      then
        if dispId = -2  //Special case when invoking __asPPyObject__
        then
          begin
            myVariant := Integer(FPythonObject); //Not so dirty!
            if Assigned(lVarResult) then
              lvarResult^ := myVariant;
          end // of if
        else
          begin
            if (dispID = -1) or (dispID = 0)  // Special case when invoking __asAtom__(-1) or IDispatch itself(0)
            then
              begin
                if Assigned(lVarResult) then
                  lVarResult^ := self.asAtom;
              end // of if
            else
              begin
                lObject := PyObject_GetAttr(FPythonObject, lObject);
                try
                  lPyArg := PyTuple_New(lParams.cArgs); // Is the tuple that contains the python method args
                  try
                    ii := lParams.cArgs - 1;
                    for i:=0 to lParams.cArgs-1 do
                    begin
                      lParam := @lParams.rgvarg^[i];
                      lParamValue := VariantAsPyObject(lParam^);
                      PyTuple_SetItem(lPyArg, ii, lParamValue);
                      dec(ii);
                    end; // of for
                    {$IFDEF DEBUG}DebugMessageFmt('  invoking with args=%s',[PyObjectAsString(lPyArg)]);{$ENDIF}
                    // Invoking python function/property - returns the value to get
                    if IsCallableType( lObject ) then
                      begin
                        lPyResult := PyObject_CallObject(lObject, lPyArg);
                        CheckError(False);
                      end // of if
                    else
                      begin
                        lPyResult := lObject;
                        Py_XIncRef(lPyResult);
                        if lParams.cArgs = 1
                        then
                          begin
                            Py_XDecRef(lPyResult);
                            lPyResult := PySequence_GetItem(lPyResult, PVariant(@lParams.rgvarg^[0])^);
                          end // of if
                        else if lParams.cArgs > 1 then
                          raise Exception.Create( 'Multi-dimensional sequences are not supported in Python' );
                      end; // of else
                  finally
                    Py_XDecRef(lPyArg);
                  end; // of try
                  {$IFDEF DEBUG}DebugMessageFmt('  python Result=%s',[PyObjectAsString(lPyREsult)]);{$ENDIF}
                  try
                    if (lPyResult <> Py_None) and (lVarResult <> nil) then // Handling result, if any
                    begin
                      if IsBasicType(lPyResult)
                      then
                        begin
                          lVarResult^ := OleVariant(PyObjectAsVariant(lPyResult)); // convert the simple value to a oleVariant
                        end // of if
                      else // if it's not a basic type, then wrap it up under a new interface
                        begin
                          lVarResult^ := TPythonAtom.Create(lPyResult).asAtom; // If so, let's create a new atom for it...Note that COM should destroy it as soon as not needed anymore
                        end; // of else
                    end; // of if
                  finally
                    Py_XDecRef(lPyResult);
                  end; // of try
                finally
                  Py_XDecRef(lObject);
                end; // of try
              end; // of else
          end // of else
      else
        if (Flags and DISPATCH_PROPERTYPUT) > 0 // We have to set a python value
        then
          begin
            lParam := @lParams.rgvarg^[0];
            lParamValue:=VariantAsPyObject(lParam^);
            try
              PyObject_SetAttr(self.PythonObject, lObject, lParamValue);
            finally
              Py_XDECREF(lParamValue);
            end; // of try
          end // of if
        else
          begin
            result := E_NOTIMPL;
            exit;
          end; // of else
    end; // of with
end;

procedure TPythonAtom.PythonAddRef;
begin
  if assigned(FPythonObject) then GetPythonEngine.Py_INCREF(FPythonObject);
end;

procedure TPythonAtom.PythonRelease;
begin
  if assigned(FPythonObject) then GetPythonEngine.Py_DECREF(FPythonObject);
end;

procedure TPythonAtom.SetPythonObject(const Value: PPyObject);
begin
  PythonRelease;
  FPythonObject := Value;
  PythonAddRef;
end;

procedure TPythonAtom.AddRef;
begin
  inherited _AddRef;
end;

procedure TPythonAtom.Release;
begin
  inherited _Release;
end;

end.
