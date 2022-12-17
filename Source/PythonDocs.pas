unit PythonDocs;

interface

uses
  System.Rtti,
  System.Types,
  System.TypInfo,
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  Xml.xmldom,
  Xml.XMLIntf,
  Xml.XMLDoc,
  Xml.omnixmldom;

type
  TDocScanResult = class(TDictionary<string, string>)
  public
    class function ExtractMemberPrefix(const AXmlMember: IXMLNode): string; overload; static; inline;
    class function BuildMemberIdentifier(const AXmlMember: IXMLNode): string; overload; static; inline;
    class function ExtractMemberPrefix(const ARttiNamedType: TRttiNamedObject): string; overload; static;
    class function BuildMemberIdentifier(const ARttiNamedType: TRttiNamedObject): string; overload; static; inline;
  end;

  {$SCOPEDENUMS ON}
  TPythonDocSource = (Xml);
  TPythonDocSymbolType = (Constant, Variable, &Procedure, &Function, Enum, &Set, &Record, &Class);
  {$SCOPEDENUMS OFF}

  /// <summary>
  ///    Loads the requested documentation from a source.
  /// </summary>
  IPythonDocLoader = interface
    ['{40FDB9F9-2EFC-45E8-8DF4-82D004B79E80}']
    /// <summary>
    ///    Loads the symbol documentation from a source.
    /// </summary>
    function LoadDoc(const ASymbolName, ADeclaringUnitName: string;
      const ASymbolType: TPythonDocSymbolType; const AStream: TStream): boolean; overload;
    /// <summary>
    ///    Loads the type documentation from a source.
    /// </summary>
    function LoadDoc(const ATypeInfo: PTypeInfo; const AStream: TStream): boolean; overload;
  end;

  /// <summary>
  ///    Retrieves all documentation from source.
  /// </summary>
  IPythonDocScanner = interface
    ['{19B7790B-E7B6-43F9-B074-34A9049FB55A}']
    /// <summary>
    ///    Scans all compatible symbols from a source.
    /// </summary>
    function ScanDocs(const ASymbolType: TPythonDocSymbolType; const AStream: TStream): TDocScanResult; overload;
    /// <summary>
    ///    Scans all compatible symbols from a source.
    /// </summary>
    function ScanDocs(const ATypeInfo: PTypeInfo; const AStream: TStream): TDocScanResult; overload;
  end;

  TPythonDocXML = class(TInterfacedObject, IPythonDocLoader, IPythonDocScanner)
  private
    class var FDefaultDirectory: string;
  private
    FDirectory: string;

    function LoadClassDoc(const AClassName: string; const AXmlDocument: IXmlDocument;
      const AStream: TStream): boolean; overload;
    function ScanClassDocs(const AStream: TStream;
      const AXmlDocument: IXmlDocument): TDocScanResult;

    function LoadDoc(const ASymbolName, ADeclaringUnitName: string;
      const ASymbolType: TPythonDocSymbolType; const AStream: TStream): boolean; overload;
    function LoadDoc(const ATypeInfo: PTypeInfo; const AStream: TStream): boolean; overload;
    function ScanDocs(const ASymbolType: TPythonDocSymbolType; const AStream: TStream): TDocScanResult; overload;
    function ScanDocs(const ATypeInfo: PTypeInfo; const AStream: TStream): TDocScanResult; overload;
  public
    constructor Create();

    property Directory: string read FDirectory write FDirectory;
    class property DefaultDirectory: string read FDefaultDirectory write FDefaultDirectory;
  end;

  TPythonDocServer = class
  private type
    TDiscoverPredicate = reference to function(const ARttiType: TRttiType; out ATypeInfo: PTypeInfo): boolean;
    TBuffer = TDictionary<PTypeInfo, IAsyncResult>;
    TPythonDocServerAsyncResult<TResult> = class(TBaseAsyncResult)
    private
      FAsyncTask: TFunc<TResult>;
      FRetVal: TResult;
    protected
      procedure Schedule; override;
      procedure AsyncDispatch; override;
    public
      constructor Create(const AContext: TObject;
        const AAsyncTask: TFunc<TResult>);

      function GetRetVal: TResult;
    end;
  private
    class var
      FInstance: TPythonDocServer;
  private
    FDocSource: TPythonDocSource;
    FBuffer: TBuffer;
    constructor Create();    
    class function GetInstance: TPythonDocServer; static;
  public
    destructor Destroy(); override;
    class destructor Destroy();
    class function NewInstance(): TObject; override;

    function BeginLoadDoc(const ATypeInfo: PTypeInfo): IAsyncResult; overload;
    function BeginLoadDoc<T>(): IAsyncResult; overload;
    function EndLoadDoc(const AAsyncResult: IAsyncResult): TDocScanResult;

    procedure Bufferize(const ATypeInfo: PTypeInfo); overload;
    procedure Bufferize<T>(); overload;
    procedure ClearBuffer();

    procedure DiscoverAndBufferizeTypes(APredicate: TDiscoverPredicate);

    property DocSource: TPythonDocSource read FDocSource write FDocSource;
    class property Instance: TPythonDocServer read GetInstance;
  end;

implementation

uses
  System.IOUtils,
  System.Threading;

{ TPythonDocXML }

constructor TPythonDocXML.Create;
begin
  inherited;
  FDirectory := FDefaultDirectory;
end;

function TPythonDocXML.LoadDoc(const ASymbolName, ADeclaringUnitName: string;
  const ASymbolType: TPythonDocSymbolType; const AStream: TStream): boolean;
var
  LFiles: TArray<string>;
  LXmlDoc: IXMLDocument;
begin
  LFiles := TDirectory.GetFiles(FDirectory,
    ADeclaringUnitName + '.xml',
    TSearchOption.soAllDirectories);

  if not Assigned(LFiles) then
    Exit(false);

  LXmlDoc := LoadXMLDocument(LFiles[Low(LFiles)]);
  LXmlDoc.Active := true;

  case ASymbolType of
    TPythonDocSymbolType.Constant:
      Result := false;
    TPythonDocSymbolType.Variable:
      Result := false;
    TPythonDocSymbolType.Procedure:
      Result := false;
    TPythonDocSymbolType.Function:
      Result := false;
    TPythonDocSymbolType.Enum:
      Result := false;
    TPythonDocSymbolType.Set:
      Result := false;
    TPythonDocSymbolType.Record:
      Result := false;
    TPythonDocSymbolType.Class:
      Result := LoadClassDoc(ASymbolName, LXmlDoc, AStream);
    else
      Result := false;
  end;
end;

function TPythonDocXML.LoadDoc(const ATypeInfo: PTypeInfo;
  const AStream: TStream): boolean;
var
  LRttiCtx: TRttiContext;
  LRttiType: TRttiType;
begin
  LRttiCtx := TRttiContext.Create();
  try
    LRttiType := LRttiCtx.GetType(ATypeInfo);
    case LRttiType.TypeKind of
      tkClass:
        Result := LoadDoc(LRttiType.Name, LRttiType.AsInstance.DeclaringUnitName, TPythonDocSymbolType.Class, AStream)
      else
        Result := false;
    end;
  finally
    LRttiCtx.Free();
  end;
end;

function TPythonDocXML.ScanDocs(const ASymbolType: TPythonDocSymbolType; const AStream: TStream): TDocScanResult;
var
  LXmlData: string;
  LXMLDoc: IXMLDocument;
begin
  SetLength(LXmlData, AStream.Size div 2);
  AStream.Read(LXmlData[1], AStream.Size);

  LXMLDoc := LoadXmlData(LXmlData);
  LXMLDoc.Active := true;

  case ASymbolType of
    TPythonDocSymbolType.Constant:
      Result := nil;
    TPythonDocSymbolType.Variable:
      Result := nil;
    TPythonDocSymbolType.Procedure:
      Result := nil;
    TPythonDocSymbolType.Function:
      Result := nil;
    TPythonDocSymbolType.Enum:
      Result := nil;
    TPythonDocSymbolType.Set:
      Result := nil;
    TPythonDocSymbolType.Record:
      Result := nil;
    TPythonDocSymbolType.Class:
      Result := ScanClassDocs(AStream, LXMLDoc);
    else
      Result := nil;
  end;
end;

function TPythonDocXML.ScanDocs(const ATypeInfo: PTypeInfo;
  const AStream: TStream): TDocScanResult;
var
  LRttiCtx: TRttiContext;
  LRttiType: TRttiType;
begin
  LRttiCtx := TRttiContext.Create();
  try
    LRttiType := LRttiCtx.GetType(ATypeInfo);
    case LRttiType.TypeKind of
      tkClass:
        Result := ScanDocs(TPythonDocSymbolType.Class, AStream)
      else
        Result := nil;
    end;
  finally
    LRttiCtx.Free();
  end;
end;

function TPythonDocXML.LoadClassDoc(const AClassName: string;
  const AXmlDocument: IXmlDocument; const AStream: TStream): boolean;
var
  LXmlDOMNodeSelect: IDOMNodeSelect;
  LXmlDOMNode: IDOMNode;
  LXmlNode: IXMLNode;
begin
  LXmlDOMNodeSelect := (AXmlDocument.DOMDocument.documentElement as IDOMNodeSelect);
  LXmlDOMNode := LXmlDOMNodeSelect.selectNode(Format('/namespace/class[@name="%s"]', [AClassName]));

  if not Assigned(LXmlDOMNode) then
    Exit(false);

  LXmlNode := TXMLNode.Create(LXmlDOMNode, nil, (AXmlDocument as IXmlDocumentAccess).DocumentObject) as IXMLNode;
  AStream.Write(LXmlNode.XML[1], ByteLength(LXmlNode.XML));

  Result := true;
end;

function TPythonDocXML.ScanClassDocs(const AStream: TStream;
  const AXmlDocument: IXmlDocument): TDocScanResult;
var
  LXmlClass: IXMLNode;
  LXmlMembers: IXmlNode;
  LXmlMember: IXmlNode;
  LXmlDevNotes: IXmlNode;
  LXmlSummary: IXmlNode;
begin
  LXmlClass := AXmlDocument.ChildNodes.FindNode('class');
  if not Assigned(LXmlClass) then
    Exit(nil);

  Result := TDocScanResult.Create();

  LXmlDevNotes := LXmlClass.ChildNodes.FindNode('devnotes');
  if Assigned(LXmlDevNotes) then begin
    LXmlSummary := LXmlDevNotes.ChildNodes.FindNode('summary');
    if Assigned(LXmlSummary) then
      Result.Add(TDocScanResult.BuildMemberIdentifier(LXmlClass), LXmlSummary.Text);
  end;

  LXmlMembers := LXmlClass.ChildNodes.FindNode('members');
  if not Assigned(LXmlMembers) then
    Exit();

  try
    for var I := 0 to LXmlMembers.ChildNodes.Count - 1 do begin
      LXmlMember := LXmlMembers.ChildNodes[I];
      LXmlDevNotes := LXmlMember.ChildNodes.FindNode('devnotes');
      if Assigned(LXmlDevNotes) then begin
        LXmlSummary := LXmlDevNotes.ChildNodes.FindNode('summary');
        if Assigned(LXmlSummary) then
          if not Result.ContainsKey(LXmlMember.NodeName + '_' + LXmlMember.GetAttribute('name')) then
            Result.Add(TDocScanResult.BuildMemberIdentifier(LXmlMember), LXmlSummary.Text);
      end;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

{ TPythonDocServer }

procedure TPythonDocServer.ClearBuffer;
begin
  FBuffer.Clear();
end;

constructor TPythonDocServer.Create;
begin
  inherited;
  FDocSource := TPythonDocSource.Xml;
  FBuffer := TBuffer.Create();
end;

destructor TPythonDocServer.Destroy;
begin
  FBuffer.Free();
end;

class function TPythonDocServer.NewInstance: TObject;
begin
  if not Assigned(FInstance) then
    FInstance := TPythonDocServer(inherited NewInstance());
  Result := FInstance;
end;

class destructor TPythonDocServer.Destroy;
begin
  FInstance.Free();
end;

procedure TPythonDocServer.DiscoverAndBufferizeTypes(APredicate: TDiscoverPredicate);
var
  LRttiContext: TRttiContext;
  LRttiPackage: TRttiPackage;
  LTypeInfo: PTypeInfo;
begin
  LRttiContext := TRttiContext.Create();
  try     
    for LRttiPackage in LRttiContext.GetPackages() do
      if (LRttiPackage.Handle = HInstance) then
        for var LRttiType in LRttiPackage.GetTypes() do
          if APredicate(LRttiType, LTypeInfo) then
            Bufferize(LTypeInfo);          
  finally
    LRttiContext.Free();
  end;
end;

class function TPythonDocServer.GetInstance: TPythonDocServer;
begin
  if not Assigned(FInstance) then
    TPythonDocServer.Create();
  Result := FInstance;
end;

function TPythonDocServer.BeginLoadDoc(
  const ATypeInfo: PTypeInfo): IAsyncResult;
var
  LDocLoader: IPythonDocLoader;
begin
  if FBuffer.TryGetValue(ATypeInfo, Result) then
    Exit;

  case FDocSource of
    TPythonDocSource.Xml:
      Result := TPythonDocServerAsyncResult<TDocScanResult>.Create(Self,
        function(): TDocScanResult
        var
          LStream: TStream;
        begin
          LStream := TMemoryStream.Create();
          try
            LDocLoader := TPythonDocXML.Create();
             if LDocLoader.LoadDoc(ATypeInfo, LStream) then begin
              LStream.Position := 0;
              Result := (LDocLoader as IPythonDocScanner).ScanDocs(ATypeInfo, LStream);
            end else
              Result := nil;
          finally
            LStream.Free();
          end;
        end).Invoke();
  end;
end;

function TPythonDocServer.BeginLoadDoc<T>: IAsyncResult;
begin
  Result := BeginLoadDoc(TypeInfo(T));
end;

procedure TPythonDocServer.Bufferize(const ATypeInfo: PTypeInfo);
begin
  FBuffer.Add(ATypeInfo, BeginLoadDoc(ATypeInfo));
end;

procedure TPythonDocServer.Bufferize<T>;
begin
  Bufferize(TypeInfo(T));
end;

function TPythonDocServer.EndLoadDoc(
  const AAsyncResult: IAsyncResult): TDocScanResult;
begin
  Result := (AAsyncResult as TPythonDocServerAsyncResult<TDocScanResult>).GetRetVal();
end;

{ TPythonDocServer.TPythonDocServerAsyncResult<TResult> }

constructor TPythonDocServer.TPythonDocServerAsyncResult<TResult>.Create(
  const AContext: TObject; const AAsyncTask: TFunc<TResult>);
begin
  inherited Create(AContext);
  FAsyncTask := AAsyncTask;
end;

procedure TPythonDocServer.TPythonDocServerAsyncResult<TResult>.AsyncDispatch;
begin
  FRetVal := FAsyncTask();
end;

function TPythonDocServer.TPythonDocServerAsyncResult<TResult>.GetRetVal: TResult;
begin
  WaitForCompletion;
  Result := FRetVal;
end;

procedure TPythonDocServer.TPythonDocServerAsyncResult<TResult>.Schedule;
begin
  TTask.Run(DoAsyncDispatch);
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
  end;
end;

initialization
  DefaultDomVendor := sOmniXmlVendor;
  
  {$IFDEF DEBUG}
  TPythonDocXML.DefaultDirectory := TPath.Combine(ExtractFilePath(GetModuleName(HInstance)), 'docs');
  {$ELSE}
  TPythonDocXML.DefaultDirectory := TPath.Combine(TDirectory.GetParent(ExcludeTrailingPathDelimiter(
    ExtractFilePath(GetModuleName(HInstance)))), 'docs');
  {$ENDIF} 
   
end.
