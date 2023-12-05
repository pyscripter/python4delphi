{$I Definition.Inc}
unit PythonDocs;
(**************************************************************************)
(*  This unit is part of the Python for Delphi (P4D) library              *)
(*  Project home: https://github.com/pyscripter/python4delphi             *)
(*                                                                        *)
(*  Project Maintainer:  PyScripter (pyscripter@gmail.com)                *)
(*  Original Authors:    Dr. Dietmar Budelsky (dbudelsky@web.de)          *)
(*                       Morgan Martinet (https://github.com/mmm-experts) *)
(*  Core developer:      Lucas Belo (lucas.belo@live.com)                 *)
(*  Contributors:        See contributors.md at project home              *)
(*                                                                        *)
(*  LICENCE and Copyright: MIT (see project home)                         *)
(**************************************************************************)

interface

uses
  System.Rtti,
  System.Types,
  System.TypInfo,
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  System.Threading,
  System.Generics.Collections,
  Xml.xmldom,
  Xml.XMLIntf,
  Xml.XMLDoc,
  Xml.omnixmldom,
  WrapDelphi;

type
  TSymbol = string;
  TDocScanResult = class;

  {$SCOPEDENUMS ON}
  TPythonDocSource = (Xml);
  TPythonDocSymbolType = (Constant, Variable, &Constructor, &Destructor, &Procedure, &Function, Enum, &Set, &Record, &Class);
  {$SCOPEDENUMS OFF}

  TDiscoverPredicate = reference to function(const ARttiType: TRttiType; out ATypeInfo: PTypeInfo): boolean;

  IPythonDocProvider = interface
    ['{4763349F-A25B-41E9-8811-2A6BD5933834}']
    /// <summary>
    ///   Find the doc of a given symbol.
    /// </summary>
    function Find(const ASymbolName: TSymbol;
      const ASymbolType: TPythonDocSymbolType;
      const ADeclaringUnitName: string): TDocScanResult;
  end;

  ITypeAnnotationProvider = interface
    ['{7359C6AE-8C5C-4279-9151-A847D1383A11}']
    /// <summary>
    ///   Creates member annotation for type inference in doc. strings.
    /// </summary>
    function CreateTypeAnnotation(const ARttiMember: TRttiMember): string;
  end;

  TPythonDocServer = class(TInterfacedObject, IDocServer)
  private
    FDocProvider: IPythonDocProvider;
    FTypeAnnotationProvider: ITypeAnnotationProvider;
    FBuffered: boolean;
    function GetDocDir(): string;
    function GetDocFile(): string;
    /// <summary>
    ///   Create member annotation for stubs generation.
    /// </summary>
    function CreateMemberAnnotation(const ARttiMember: TRttiMember): string;
    /// <summary>
    ///   Create member docs based on parent class or its descendents.
    /// </summary>
    function CreateMemberDocStr(const AParentInfo: PTypeInfo;
      const ARttiMember: TRttiMember): string;
    /// <summary>
    ///   Combine docstring and type annotation.
    /// </summary>
    function CombineDocStrAndTypeAnnotation(const ADocString, ATypeAnnotation: string): string; overload;
    /// <summary>
    ///   Combine  doc. strings and type annotation.
    /// </summary>
    function CombineDocStrAndTypeAnnotation(const ARttiMember: TRttiMember): string; overload;
  public
    constructor Create();
    /// <summary>
    ///   Bufferize all symbols.
    /// </summary>
    procedure Initialize;
    /// <summary>
    ///   Check if symbols have been bufferized.
    /// </summary>
    function Initialized: Boolean;
    /// <summary>
    ///    Clear all symbols info from buffer.
    /// </summary>
    procedure Finalize;
    /// <summary>
    ///    Reads the docs of a symbol.
    /// </summary>
    function ReadTypeDocStr(const ASymbolName: TSymbol;
      const ASymbolType: TPythonDocSymbolType;
      const ADeclaringUnitName: string; out ADocStr: string): boolean; overload;
    /// <summary>
    ///    Reads the docs of a type.
    /// </summary>
    function ReadTypeDocStr(ATypeInfo: PTypeInfo; out ADocStr: string): boolean; overload;
    /// <summary>
    ///    Reads the docs of a member of a type.
    /// </summary>
    function ReadMemberDocStr(AParentInfo: PTypeInfo; AMember: TRttiMember;
      out ADocStr: string): boolean; overload;
    /// <summary>
    ///    Reads the docs of a member of a type.
    /// </summary>
    function ReadMemberDocStr(AMember: TRttiMember; out ADocStr: string): boolean; overload;
    /// <summary>
    ///    Doc provider instance.
    /// </summary>
    property DocProvider: IPythonDocProvider read FDocProvider write FDocProvider;
    property TypeAnnotationProvider: ITypeAnnotationProvider read FTypeAnnotationProvider write FTypeAnnotationProvider;
  end;

  TPythonDocXML = class(TInterfacedObject, IPythonDocProvider)
  private type
    TDiscoveredDocs = TDocScanResult;
    TDiscoveredSymbols = TObjectDictionary<string, TDiscoveredDocs>;
    TDiscoveredFiles = TObjectDictionary<string, TDiscoveredSymbols>;
  private
    class var FDiscoveredFiles: TDiscoveredFiles;
    /// <summary>
    ///    Creates a XML document instance with the necessary settings.
    /// </summary>
    class function CreateXmlDoc(const AFileName: string): IXMLDocument;
    /// <summary>
    ///    Create a buffer with all nodes of the XML file.
    /// </summary>
    class procedure BufferizeClasses(const AClassNodes: IXMLNode; const ASymbolMap: TDiscoveredSymbols);
    class procedure BufferizeMembers(const AMemberNodes: IXMLNode; const ADocMap: TDiscoveredDocs);
  public
    class constructor Create();
    class destructor Destroy();
    /// <summary>
    ///   Find a symbol in the buffered map
    /// </summary>
    function Find(const ASymbolName: TSymbol;
      const ASymbolType: TPythonDocSymbolType;
      const ADeclaringUnitName: string): TDocScanResult;
    /// <summary>
    ///    Bufferizes the XML doc file.
    /// </summary>
    class procedure Bufferize(const AFileName: string);
    /// <summary>
    ///    Clear all discovered files with buffered xml from buffer.
    /// </summary>
    class procedure ClearBuffer();
  end;

  /// <summary>
  ///   Using standard function signature for methods.
  ///   Signature is a string of the format
  ///     <function_name>(<signature>) -> <return type>
  ///   or perhaps without the return type.

  ///   Using Google Docstrings Style for fields and properties. It is a string of the format
  ///     <type: docstring>
  ///   See: https://sphinxcontrib-napoleon.readthedocs.io/en/latest/example_google.html
  /// </summary>
  TMyPyTypeAnnotation = class(TInterfacedObject, ITypeAnnotationProvider)
  private
    function TranslateType(const ARttiObject: TRttiObject): string;
    /// <summary>
    ///   Creates method annotation for type inference in doc. strings.
    /// </summary>
    function GetMethodTypeAnnotation(
      const ARttiMethod: TRttiMethod): string;
    /// <summary>
    ///   Creates field annotation for type inference in doc. strings.
    /// </summary>
    function GetFieldTypeAnnotation(const ARttiField: TRttiField): string;
    /// <summary>
    ///   Creates property annotation for type inference in doc. strings.
    /// </summary>
    function GetPropertyTypeAnnotation(
      const ARttiProperty: TRttiProperty): string;
    /// <summary>
    ///   Creates indexed property annotation for type inference in doc. strings.
    /// </summary>
    function GetIndexedPropertyTypeAnnotation(
      const ARttiIndexedProperty: TRttiIndexedProperty): string;
  public
    /// <summary>
    ///   Creates member annotation for type inference in doc. strings.
    /// </summary>
    function CreateTypeAnnotation(const ARttiMember: TRttiMember): string;
  end;

  TDocScanResult = class(TDictionary<string, string>)
  public
    class function ExtractMemberPrefix(const AXmlMember: IXMLNode): string; overload; static; inline;
    class function BuildMemberIdentifier(const AXmlMember: IXMLNode): string; overload; static; inline;
    class function ExtractMemberPrefix(const ARttiNamedType: TRttiNamedObject): string; overload; static;
    class function BuildMemberIdentifier(const ARttiNamedType: TRttiNamedObject): string; overload; static; inline;
    class function BuildMemberIdentifier(const ATypeInfo: PTypeInfo): string; overload; static; inline;
    class function BuildMemberIdentifier(const ASymbolName: string; const ASymbolType: TPythonDocSymbolType): string; overload; static; inline;
  end;

const
  DOC_DIR_NAME = 'doc';
  DOC_FILE_NAME = 'docs.xml';

implementation

uses
  System.IOUtils,
  System.StrUtils;

{ TPythonDocServer }

constructor TPythonDocServer.Create;
begin
  inherited;
  FDocProvider := TPythonDocXML.Create();
  FTypeAnnotationProvider := TMyPyTypeAnnotation.Create();
end;

function TPythonDocServer.GetDocDir(): string;
begin
  {$IFDEF ANDROID}
  Exit(TPath.GetDocumentsPath());
  {$ENDIF ANDROID}

  Result := TPath.Combine(
    TDirectory.GetParent(ExcludeTrailingPathDelimiter(
      ExtractFilePath(GetModuleName(HInstance)))), DOC_DIR_NAME);
end;

function TPythonDocServer.GetDocFile: string;
begin
  Result := TPath.Combine(GetDocDir(), DOC_FILE_NAME);
end;

procedure TPythonDocServer.Initialize;
begin
  TPythonDocXML.Bufferize(GetDocFile());
  FBuffered := true;
end;

function TPythonDocServer.Initialized: Boolean;
begin
  Result := FBuffered;
end;

procedure TPythonDocServer.Finalize;
begin
  FBuffered := false;
  TPythonDocXML.ClearBuffer();
end;

function TPythonDocServer.CreateMemberDocStr(
  const AParentInfo: PTypeInfo; const ARttiMember: TRttiMember): string;
begin
  if not Assigned(AParentInfo) then
    Exit(String.Empty);

  if not ReadMemberDocStr(AParentInfo, ARttiMember, Result) then
    if not Assigned(GetTypeData(AParentInfo)^.ParentInfo) then
      Result := String.Empty
    else
      Result := CreateMemberDocStr(GetTypeData(AParentInfo)^.ParentInfo^, ARttiMember);
end;

function TPythonDocServer.CreateMemberAnnotation(
  const ARttiMember: TRttiMember): string;
begin
  Result := FTypeAnnotationProvider.CreateTypeAnnotation(ARttiMember);
end;

function TPythonDocServer.CombineDocStrAndTypeAnnotation(const ADocString,
  ATypeAnnotation: string): string;
begin
  //We only have the docstring or neither
  if ATypeAnnotation.IsEmpty() then
    Result := ADocString
  //We only have the type annotation
  else if ADocString.IsEmpty() then
    Result := ATypeAnnotation
  //We have both values
  else
    Result := ATypeAnnotation + ADocString;
end;

function TPythonDocServer.CombineDocStrAndTypeAnnotation(const ARttiMember: TRttiMember): string;
begin
  Result := CombineDocStrAndTypeAnnotation(
    CreateMemberDocStr(ARttiMember.Parent.Handle, ARttiMember),
    CreateMemberAnnotation(ARttiMember));
end;

function TPythonDocServer.ReadTypeDocStr(const ASymbolName: TSymbol;
  const ASymbolType: TPythonDocSymbolType;
  const ADeclaringUnitName: string; out ADocStr: string): boolean;
var
  LDocs: TDocScanResult;
begin
  LDocs := FDocProvider.Find(ASymbolName, ASymbolType, ADeclaringUnitName);
  if not Assigned(LDocs) then
    Exit(false);

  Result := LDocs.TryGetValue(
    TDocScanResult.BuildMemberIdentifier(ASymbolName, ASymbolType), ADocStr);
end;

function TPythonDocServer.ReadTypeDocStr(ATypeInfo: PTypeInfo;
  out ADocStr: string): boolean;
begin
  case ATypeInfo^.Kind of
    tkClass: Result := ReadTypeDocStr(TSymbol(ATypeInfo^.Name),
      TPythonDocSymbolType.Class,
      String(ATypeInfo^.TypeData^.UnitName), ADocStr);
    //These docs will be enhanced as needed
    else raise ENotSupportedException.Create('Type doesn''t support documentation.');
  end;
end;

function TPythonDocServer.ReadMemberDocStr(AParentInfo: PTypeInfo;
  AMember: TRttiMember; out ADocStr: string): boolean;
var
  LDocs: TDocScanResult;
begin
  case AParentInfo^.Kind of
    tkClass: begin
      LDocs := FDocProvider.Find(
        TSymbol(AParentInfo^.Name),
        TPythonDocSymbolType.Class,
        String(AParentInfo^.TypeData^.UnitName));

      if not Assigned(LDocs) then
        Exit(false);

      Result := LDocs.TryGetValue(
        TDocScanResult.BuildMemberIdentifier(AMember), ADocStr);
    end;
    //These docs will be enhanced as needed
    else raise ENotSupportedException.Create('Type doesn''t support documentation.');
  end;
end;

function TPythonDocServer.ReadMemberDocStr(AMember: TRttiMember;
  out ADocStr: string): boolean;
begin
  ADocStr := CombineDocStrAndTypeAnnotation(AMember);
  Result := not ADocStr.IsEmpty;
end;

{ TPythonDocXML }

class constructor TPythonDocXML.Create;
begin
  FDiscoveredFiles := TDiscoveredFiles.Create([doOwnsValues]);
end;

class destructor TPythonDocXML.Destroy;
begin
  FDiscoveredFiles.Free();
end;

function TPythonDocXML.Find(const ASymbolName: TSymbol;
  const ASymbolType: TPythonDocSymbolType;
  const ADeclaringUnitName: string): TDocScanResult;
var
  LSymbols: TDiscoveredSymbols;
begin
  //Do we have the given unit bufferized?
  if not FDiscoveredFiles.TryGetValue(ADeclaringUnitName, LSymbols) then
    Exit(nil);

  case ASymbolType of
    TPythonDocSymbolType.Class:
      //Do we have the given symbol bufferized?
      if LSymbols.ContainsKey(ASymbolName) then
        Exit(LSymbols[ASymbolName]);
    //These docs will be enhanced as needed
    else raise ENotSupportedException.Create('Type doesn''t support documentation.');
  end;

  Result := nil;
end;

class function TPythonDocXML.CreateXmlDoc(const AFileName: string): IXMLDocument;
var
  LXMLDocument: TXMLDocument;
begin
  LXMLDocument := TXMLDocument.Create(nil);
  try
    LXMLDocument.DOMVendor := GetDOMVendor(sOmniXmlVendor);
    LXMLDocument.FileName := AFileName;
    LXMLDocument.Active := true;
  except
    LXMLDocument.Free();
    raise;
  end;

  Result := LXMLDocument;
end;

class procedure TPythonDocXML.Bufferize(const AFileName: string);
var
  LXMLDoc: IXMLDocument;
  LXmlDOMNodeSelect: IDOMNodeSelect;
  LXmlDOMNodes: IDOMNodeList;
  I: Integer;
  LSymbols: TDiscoveredSymbols;
begin
  if not TFile.Exists(AFileName) then
    Exit;

  LXMLDoc := CreateXmlDoc(AFileName);

  LXmlDOMNodeSelect := (LXMLDoc.DOMDocument.documentElement as IDOMNodeSelect);
  if not Assigned(LXmlDOMNodeSelect) then
    Exit;

  //We currently only support classes
  LXmlDOMNodes := LXmlDOMNodeSelect.selectNodes('/docs/class');
  if not Assigned(LXmlDOMNodes) then
    Exit;

  //Bufferize all classes
  for I := 0 to Pred(LXmlDOMNodes.Length) do begin
    //This must be fast
    if not FDiscoveredFiles.TryGetValue(
      LXmlDOMNodes.Get_Item(I).attributes.getNamedItem('unit').nodeValue,
      LSymbols) then
    begin
      LSymbols := TDiscoveredSymbols.Create([doOwnsValues]);
      FDiscoveredFiles.Add(
        LXmlDOMNodes.Get_Item(I).attributes.getNamedItem('unit').nodeValue,
        LSymbols);
    end;
    //Add a class and its members into the buffer
    BufferizeClasses(TXMLNode.Create(LXmlDOMNodes.Get_Item(I), nil,
      LXMLDoc as TXMLDocument) as IXMLNode, LSymbols);
  end;
end;

class procedure TPythonDocXML.BufferizeClasses(const AClassNodes: IXMLNode;
  const ASymbolMap: TDiscoveredSymbols);
var
  LDiscoveredDocs: TDiscoveredDocs;
  LXmlDocStr: IXMLNode;
  LXmlMembers: IXMLNode;
begin
  LDiscoveredDocs := TDiscoveredDocs.Create();
  try
    //Add a discovered doc map to the class
    ASymbolMap.Add(AClassNodes.GetAttribute('name'), LDiscoveredDocs);

    //Look for class doc
    LXmlDocStr := AClassNodes.ChildNodes.FindNode('docstr');
    if Assigned(LXmlDocStr) then
      LDiscoveredDocs.Add(
      TDocScanResult.BuildMemberIdentifier(AClassNodes), LXmlDocStr.Text);

    //Scan class members
    LXmlMembers := AClassNodes.ChildNodes.FindNode('members');
    if Assigned(LXmlMembers) then
      BufferizeMembers(LXmlMembers, LDiscoveredDocs);
  except
    FreeAndNil(LDiscoveredDocs);
    raise;
  end;
end;

class procedure TPythonDocXML.BufferizeMembers(const AMemberNodes: IXMLNode;
  const ADocMap: TDiscoveredDocs);
var
  I: Integer;
  LIdentifier: string;
begin
  for I := 0 to AMemberNodes.ChildNodes.Count - 1 do begin
    LIdentifier := TDocScanResult.BuildMemberIdentifier(AMemberNodes.ChildNodes[I]);
    if not ADocMap.ContainsKey(LIdentifier) then
      ADocMap.Add(LIdentifier, AMemberNodes.ChildNodes[I].ChildNodes.FindNode('docstr').Text)
    //Concat docstr for overloaded methods. We are considering any duplicates as overloads.
    else begin
      ADocMap[LIdentifier] := ADocMap[LIdentifier]
        + #13#10
        + AMemberNodes.ChildNodes[I].ChildNodes.FindNode('docstr').Text;
    end;
  end;
end;

class procedure TPythonDocXML.ClearBuffer;
begin
  FDiscoveredFiles.Clear();
end;

{ TDocScanResult }

class function TDocScanResult.BuildMemberIdentifier(
  const AXmlMember: IXMLNode): string;
begin
  Result := ExtractMemberPrefix(AXmlMember) + '_' + AXmlMember.GetAttribute('name');
end;

class function TDocScanResult.BuildMemberIdentifier(
  const ARttiNamedType: TRttiNamedObject): string;
begin
  Result := TDocScanResult.ExtractMemberPrefix(ARttiNamedType) + '_' + ARttiNamedType.Name;
end;

class function TDocScanResult.ExtractMemberPrefix(
  const AXmlMember: IXMLNode): string;
begin
  Result := AXmlMember.NodeName;
end;

class function TDocScanResult.BuildMemberIdentifier(
  const ATypeInfo: PTypeInfo): string;
var
  LRttiContext: TRttiContext;
begin
  LRttiContext := TRttiContext.Create();
  try
    Result := BuildMemberIdentifier(LRttiContext.GetType(ATypeInfo));
  finally
    LRttiContext.Free();
  end;
end;

class function TDocScanResult.ExtractMemberPrefix(
  const ARttiNamedType: TRttiNamedObject): string;
begin
  Result := String.Empty;
  if ARttiNamedType is TRttiType then begin
    if TRttiType(ARttiNamedType).IsInstance then
      Result := 'class';
  end else if ARttiNamedType is TRttiMethod then begin
    if TRttiMethod(ARttiNamedType).IsConstructor then
      Result := 'constructor'
    else if TRttiMethod(ARttiNamedType).IsDestructor then
      Result := 'destructor'
    else if Assigned(TRttiMethod(ARttiNamedType).ReturnType) then
      Result := 'function'
    else
      Result := 'procedure';
  end else if ARttiNamedType is TRttiField then
    Result := 'field'
  else if ARttiNamedType is TRttiProperty then
    Result := 'property'
  else if ARttiNamedType is TRttiIndexedProperty then
    Result := 'indexed_property';
end;

class function TDocScanResult.BuildMemberIdentifier(const ASymbolName: string;
  const ASymbolType: TPythonDocSymbolType): string;
begin
  case ASymbolType of
    TPythonDocSymbolType.Constant:
      Result := 'const_' + ASymbolName;
    TPythonDocSymbolType.Variable:
      Result := 'var_' + ASymbolName;
    TPythonDocSymbolType.Constructor:
      Result := 'constructor_' + ASymbolName;
    TPythonDocSymbolType.Destructor:
      Result := 'destructor_' + ASymbolName;
    TPythonDocSymbolType.Procedure:
      Result := 'procedure_' + ASymbolName;
    TPythonDocSymbolType.Function:
      Result := 'function_';
    TPythonDocSymbolType.Enum:
      Result := 'enum_' + ASymbolName;
    TPythonDocSymbolType.Set:
      Result := 'set_' + ASymbolName;
    TPythonDocSymbolType.Record:
      Result := 'record_' + ASymbolName;
    TPythonDocSymbolType.Class:
      Result := 'class_' + ASymbolName;
  end;
end;

{ TMyPyTypeAnnotation }

function TMyPyTypeAnnotation.TranslateType(
  const ARttiObject: TRttiObject): string;

  function TranslateMethod(const ARttiMethod: TRttiMethod): string;
  const
    METHOD_DOC_STR_PATTERN = 'Callable[[%s], %s]';
  var
    LParams: TArray<string>;
    LParam: TRttiParameter;
  begin
    LParams := nil;
    for LParam in ARttiMethod.GetParameters() do
      LParams := LParams + [TranslateType(LParam.ParamType)];

    Result := Format(METHOD_DOC_STR_PATTERN, [
      String.Join(', ', LParams),
      TranslateType(ARttiMethod.ReturnType)]);
  end;

  function TranslateInvokable(const ARttiInvokableType: TRttiInvokableType): string;
  const
    INVOKABLE_DOC_STR_PATTERN = 'Callable[[%s], %s]';
  var
    LParams: TArray<string>;
    LParam: TRttiParameter;
  begin
    LParams := nil;
    for LParam in ARttiInvokableType.GetParameters() do
      LParams := LParams + [TranslateType(LParam.ParamType)];

    Result := Format(INVOKABLE_DOC_STR_PATTERN, [
      String.Join(', ', LParams),
      TranslateType(ARttiInvokableType.ReturnType)]);
  end;

begin
  if not Assigned(ARttiObject) then
    Result := 'None'
  else if PTypeInfo(TypeInfo(boolean)) = ARttiObject.Handle then
    Result := 'bool'
  else if ARttiObject.InheritsFrom(TRttiMethod) then
    Result := TranslateMethod(ARttiObject as TRttiMethod)
  else if ARttiObject.InheritsFrom(TRttiInvokableType) then
    Result := TranslateInvokable(ARttiObject as TRttiInvokableType)
  else if ARttiObject.InheritsFrom(TRttiType) then
    case TRttiType(ARttiObject).TypeKind of
      tkUnknown,
      tkVariant,
      tkSet, tkEnumeration,
      tkClass, tkMethod, tkProcedure, tkClassRef, tkPointer,
      tkRecord, tkMRecord,
      tkInterface:
        Result := TRttiType(ARttiObject).Name.Replace('T', '', []);
      tkInteger, tkInt64:
        Result := 'int';
      tkChar:
        Result := 'ansichr(bytes)';
      tkWChar:
        Result := 'unicodechr(str)';
      tkFloat:
        Result := 'float';
      tkString, tkUString, tkWString:
        Result := 'str';
      tkLString:
        Result := 'ansistr(bytes)';
      tkArray, tkDynArray:
        Result := 'tuple';
    end;
end;

function TMyPyTypeAnnotation.GetMethodTypeAnnotation(
  const ARttiMethod: TRttiMethod): string;
const
  METHOD_DOC_STR_PATTERN = '%s.%s(%s)';
var
  LArgsStr: string;
  LRttiParameter: TRttiParameter;
begin
  if (Length(ARttiMethod.GetParameters) = 0) then
    Exit(String.Empty);

  LArgsStr := String.Empty;
  for LRttiParameter in ARttiMethod.GetParameters do begin
    if not LArgsStr.IsEmpty then
      LArgsStr := LArgsStr + ', ';

    if not Assigned(LRttiParameter.ParamType) then
      LArgsStr := LArgsStr + LRttiParameter.Name
    else
      LArgsStr := LArgsStr
        + LRttiParameter.Name
        + ': '
        + TranslateType(LRttiParameter.ParamType);
  end;

  Result := String.Format(METHOD_DOC_STR_PATTERN, [
    ARttiMethod.Parent.Name, ARttiMethod.Name, LArgsStr]);

  if Assigned(ARttiMethod.ReturnType) then
    Result := Result
      + ' -> '
      + TranslateType(ARttiMethod.ReturnType)
  else
    Result := Result + ' -> None';

  if not Result.IsEmpty() then
    Result := Result + #10;
end;

function TMyPyTypeAnnotation.GetFieldTypeAnnotation(
  const ARttiField: TRttiField): string;
const
  FIELD_DOC_STR_PATTERN = '%s: ';
begin
  Result := Format(FIELD_DOC_STR_PATTERN, [
    TranslateType(TRttiField(ARttiField).FieldType)]);
end;

function TMyPyTypeAnnotation.GetPropertyTypeAnnotation(
  const ARttiProperty: TRttiProperty): string;
const
  PROPERTY_DOC_STR_PATTERN = '%s: ';
begin
  Result := Format(PROPERTY_DOC_STR_PATTERN, [
    TranslateType(ARttiProperty.PropertyType)]);
end;

function TMyPyTypeAnnotation.GetIndexedPropertyTypeAnnotation(
  const ARttiIndexedProperty: TRttiIndexedProperty): string;
const
  INDEXED_PROPERTY_DOC_STR_PATTERN = '%s: ';
begin
  Result := Format(INDEXED_PROPERTY_DOC_STR_PATTERN, [
    TranslateType(ARttiIndexedProperty.PropertyType)]);
end;

function TMyPyTypeAnnotation.CreateTypeAnnotation(
  const ARttiMember: TRttiMember): string;
begin
  if ARttiMember.InheritsFrom(TRttiMethod) then
    Result := GetMethodTypeAnnotation(TRttiMethod(ARttiMember))
  else if ARttiMember.InheritsFrom(TRttiField) then
    Result := GetFieldTypeAnnotation(TRttiField(ARttiMember))
  else if ARttiMember.InheritsFrom(TRttiProperty) then
    Result := GetPropertyTypeAnnotation(TRttiProperty(ARttiMember))
  else if ARttiMember.InheritsFrom(TRttiIndexedProperty) then
    Result := GetIndexedPropertyTypeAnnotation(TRttiIndexedProperty(ARttiMember))
  else
    Result := String.Empty;
end;

end.
