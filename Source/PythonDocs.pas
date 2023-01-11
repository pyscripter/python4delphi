{$I Definition.Inc}
unit PythonDocs;

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
  Xml.omnixmldom;

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

  TPythonDocServer = class
  private
    class var FInstance: TPythonDocServer;
    const DOC_DIR_NAME = 'doc';
    const DOC_FILE_NAME = 'docs.xml';
  private
    FProvider: IPythonDocProvider;
    constructor Create();
    class function GetInstance: TPythonDocServer; static;
    class function GetDocDir(): string; static;
    class function GetDocFile(): string; static;
  public
    class destructor Destroy();
  public
    /// <summary>
    ///   Bufferize all symbols.
    /// </summary>
    procedure Bufferize();
    /// <summary>
    ///    Clear all symbol info off buffer.
    /// </summary>
    procedure ClearBuffer();

    /// <summary>
    ///    Reads the docs of a symbol.
    /// </summary>
    function ReadTypeDocStr(const ASymbolName: TSymbol;
      const ASymbolType: TPythonDocSymbolType;
      const ADeclaringUnitName: string; out ADocStr: string): boolean; overload;
    /// <summary>
    ///    Reads the docs of a type.
    /// </summary>
    function ReadTypeDocStr(const ATypeInfo: PTypeInfo; out ADocStr: string): boolean; overload;
    /// <summary>
    ///    Reads the docs of a type.
    /// </summary>
    function ReadTypeDocStr<T>(out ADocStr: string): boolean; overload;

    /// <summary>
    ///    Reads the docs of a member of a type.
    /// </summary>
    function ReadMemberDocStr(const AParent: PTypeInfo; const AMember: TRttiMember; out ADocStr: string): boolean; overload;
    /// <summary>
    ///    Reads the docs of a member of a type.
    /// </summary>
    function ReadMemberDocStr<T>(const AMember: TRttiMember; out ADocStr: string): boolean; overload;
    /// <summary>
    ///    Doc provider instance.
    /// </summary>
    property Provider: IPythonDocProvider read FProvider write FProvider;

    class property Instance: TPythonDocServer read GetInstance;
  end;

  TPythonDocXML = class(TInterfacedObject, IPythonDocProvider)
  private type
    TDiscoveredDocs = TDocScanResult;
    TDiscoveredSymbols = TObjectDictionary<string, TDiscoveredDocs>;
    TDiscoveredFiles = TObjectDictionary<string, TDiscoveredSymbols>;
  private
    class var FDiscoveredFiles: TDiscoveredFiles;
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

  TDocScanResult = class(TDictionary<string, string>)
  public
    class function ExtractMemberPrefix(const AXmlMember: IXMLNode): string; overload; static; inline;
    class function BuildMemberIdentifier(const AXmlMember: IXMLNode): string; overload; static; inline;
    class function ExtractMemberPrefix(const ARttiNamedType: TRttiNamedObject): string; overload; static;
    class function BuildMemberIdentifier(const ARttiNamedType: TRttiNamedObject): string; overload; static; inline;
    class function BuildMemberIdentifier(const ATypeInfo: PTypeInfo): string; overload; static; inline;
    class function BuildMemberIdentifier(const ASymbolName: string; const ASymbolType: TPythonDocSymbolType): string; overload; static; inline;
  end;

implementation

uses
  System.IOUtils,
  System.StrUtils;

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
    else raise ENotSupportedException.Create('Type doesn''t supports documentation.');
  end;

  Result := nil;
end;

class procedure TPythonDocXML.Bufferize(const AFileName: string);
var
  LXMLDoc: IXMLDocument;
  LXmlDOMNodeSelect: IDOMNodeSelect;
  LXmlDOMNodes: IDOMNodeList;
  I: Integer;
  LSymbols: TDiscoveredSymbols;
begin
  //We expect UTF-8 XML files
  LXMLDoc := LoadXmlDocument(AFileName);

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

{ TPythonDocServer }

constructor TPythonDocServer.Create;
begin
  inherited;
  FProvider := TPythonDocXML.Create();
end;

class destructor TPythonDocServer.Destroy;
begin
  FInstance.Free();
end;

class function TPythonDocServer.GetInstance: TPythonDocServer;
begin
  if not Assigned(FInstance) then
    FInstance := TPythonDocServer.Create();
  Result := FInstance;
end;

class function TPythonDocServer.GetDocDir(): string;
begin
  {$IFDEF DEBUG}
  Result := TPath.Combine(ExtractFilePath(
    GetModuleName(HInstance)), DOC_DIR_NAME);
  {$ELSE}
  Result := TPath.Combine(
    TDirectory.GetParent(ExcludeTrailingPathDelimiter(
      ExtractFilePath(GetModuleName(HInstance)))), DOC_DIR_NAME);
  {$ENDIF}
end;

class function TPythonDocServer.GetDocFile: string;
begin
  Result := TPath.Combine(TPythonDocServer.GetDocDir(), DOC_FILE_NAME);
end;

procedure TPythonDocServer.Bufferize;
begin
  TPythonDocXML.Bufferize(TPythonDocServer.GetDocFile());
end;

procedure TPythonDocServer.ClearBuffer;
begin
  TPythonDocXML.ClearBuffer();
end;

function TPythonDocServer.ReadTypeDocStr(const ASymbolName: TSymbol;
  const ASymbolType: TPythonDocSymbolType;
  const ADeclaringUnitName: string; out ADocStr: string): boolean;
var
  LDocs: TDocScanResult;
begin
  LDocs := FProvider.Find(ASymbolName, ASymbolType, ADeclaringUnitName);
  if not Assigned(LDocs) then
    Exit(false);

  Result := LDocs.TryGetValue(
    TDocScanResult.BuildMemberIdentifier(ASymbolName, ASymbolType), ADocStr);
end;

function TPythonDocServer.ReadTypeDocStr(const ATypeInfo: PTypeInfo;
  out ADocStr: string): boolean;
begin
  case ATypeInfo^.Kind of
    tkClass: Result := ReadTypeDocStr(TSymbol(ATypeInfo^.Name),
      TPythonDocSymbolType.Class,
      String(ATypeInfo^.TypeData^.UnitName), ADocStr);
    //These docs will be enhanced as needed
    else raise ENotSupportedException.Create('Type doesn''t supports documentation.');
  end;
end;

function TPythonDocServer.ReadTypeDocStr<T>(out ADocStr: string): boolean;
begin
  Result := ReadTypeDocStr(TypeInfo(T), ADocStr);
end;

function TPythonDocServer.ReadMemberDocStr(const AParent: PTypeInfo;
  const AMember: TRttiMember; out ADocStr: string): boolean;
var
  LDocs: TDocScanResult;
begin
  case AParent^.Kind of
    tkClass: begin
      LDocs := FProvider.Find(TSymbol(AParent^.Name), TPythonDocSymbolType.Class,
        String(AParent^.TypeData^.UnitName));

      if not Assigned(LDocs) then
        Exit(false);

      Result := LDocs.TryGetValue(
        TDocScanResult.BuildMemberIdentifier(AMember), ADocStr);
    end;
    //These docs will be enhanced as needed
    else raise ENotSupportedException.Create('Type doesn''t supports documentation.');
  end;
end;

function TPythonDocServer.ReadMemberDocStr<T>(const AMember: TRttiMember;
  out ADocStr: string): boolean;
begin
  Result := ReadMemberDocStr(TypeInfo(T), AMember, ADocStr);
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
    Result := 'property';
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

initialization
  DefaultDomVendor := sOmniXmlVendor;
  TPythonDocServer.Instance.Bufferize();

end.
