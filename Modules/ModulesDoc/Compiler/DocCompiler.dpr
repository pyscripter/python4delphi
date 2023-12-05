(********************************************************
 *          XML Doc Compiler for P4D Modules            *
 *                                                      *
 * Copyright (c) 2023 by Lucas Moura Belo - lmbelo      *
 * Licensed under the MIT License                       *
 *                                                      *
 *                                                      *
 * Generates a minimalistic XML doc as a result of a    *
 * group of XML doc files.                              *
 *                                                      *
 *                                                      *
 * For full license text and more information visit:    *
 * https://github.com/Embarcadero/python4delphi         *
 ********************************************************)
program DocCompiler;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.Rtti,
  System.TypInfo,
  System.Classes,
  System.IOUtils,
  System.SysUtils,
  System.Generics.Collections,
  Xml.xmldom,
  Xml.XMLIntf,
  Xml.XMLDoc,
  Xml.omnixmldom;

type
  TPythonDocXml = class
  public type
    TDocStr = record
      Name: string;
      &Type: string;
      DocStr: string;
      Children: TArray<TDocStr>;
    end;
    TDiscoveredDocs = TList<TDocStr>;
    TDiscoveredFiles = TObjectDictionary<string, TDiscoveredDocs>;
  private
    FDiscoveredFiles: TDiscoveredFiles;
    function Bufferize(const AFileName: string; const ADirectory: string): TDiscoveredDocs;
    procedure BufferizeClass(const AClassNodes: IXMLNode; const AList: TDiscoveredDocs);
    function BufferizeMembers(const AMemberNodes: IXMLNode): TArray<TDocStr>;
  public
    constructor Create();
    destructor Destroy(); override;

    /// <summary>
    ///    Automatically discover and bufferize files and its xml content.
    /// </summary>
    procedure DiscoverAndBufferize(const ADirectory: string);

    function LeanUp(): IXMLDocument;
  end;

{ TPythonDocXml }

constructor TPythonDocXml.Create;
begin
  FDiscoveredFiles := TDiscoveredFiles.Create([doOwnsValues]);
end;

destructor TPythonDocXml.Destroy;
begin
  FDiscoveredFiles.Free();
  inherited;
end;

function TPythonDocXml.Bufferize(const AFileName,
  ADirectory: string): TDiscoveredDocs;
var
  LXMLDoc: IXMLDocument;
  LXmlDOMNodeSelect: IDOMNodeSelect;
  LXmlDOMNodes: IDOMNodeList;
  I: Integer;
begin
  //We expect UTF-8 XML files
  LXMLDoc := LoadXmlDocument(AFileName);

  LXmlDOMNodeSelect := (LXMLDoc.DOMDocument.documentElement as IDOMNodeSelect);
  if not Assigned(LXmlDOMNodeSelect) then
    Exit(nil);

  //We currently only support classes
  LXmlDOMNodes := LXmlDOMNodeSelect.selectNodes('/namespace/class');
  if not Assigned(LXmlDOMNodes) then
    Exit(nil);

  Result := TDiscoveredDocs.Create();
  try
    //Bufferize all classes
    for I := 0 to Pred(LXmlDOMNodes.Length) do
      BufferizeClass(TXMLNode.Create(LXmlDOMNodes.Get_Item(I), nil,
        LXMLDoc as TXMLDocument) as IXMLNode, Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TPythonDocXml.BufferizeClass(const AClassNodes: IXMLNode;
  const AList: TDiscoveredDocs);
var
  LDiscoveredDocs: TDiscoveredDocs;
  LXmlDevNotes: IXMLNode;
  LXmlSummary: IXMLNode;
  LXmlMembers: IXMLNode;
  LDocStr: TDocStr;
begin
  LDocStr.Name := AClassNodes.GetAttribute('name');
  LDocStr.&Type := AClassNodes.NodeName;

  //Look for class doc
  LXmlDevNotes := AClassNodes.ChildNodes.FindNode('devnotes');
  if Assigned(LXmlDevNotes) then begin
    LXmlSummary := LXmlDevNotes.ChildNodes.FindNode('summary');
    if Assigned(LXmlSummary) then begin
      LDocStr.DocStr := LXmlSummary.Text;
    end;
  end;

  //Scan class members
  LXmlMembers := AClassNodes.ChildNodes.FindNode('members');
  if Assigned(LXmlMembers) then
    LDocStr.Children := BufferizeMembers(LXmlMembers)
  else
    LDocStr.Children := [];

  if not LDocStr.DocStr.IsEmpty() or Assigned(LDocStr.Children) then
    AList.Add(LDocStr);
end;

function TPythonDocXml.BufferizeMembers(
  const AMemberNodes: IXMLNode): TArray<TDocStr>;
var
  I: Integer;
  LXmlMember: IXMLNode;
  LXmlDevNotes: IXMLNode;
  LXmlSummary: IXMLNode;
  LIdentifier: string;
  LDocStr: TDocStr;
begin
  Result := [];
  for I := 0 to AMemberNodes.ChildNodes.Count - 1 do begin
    LXmlMember := AMemberNodes.ChildNodes[I];
    //Look for member doc
    LXmlDevNotes := LXmlMember.ChildNodes.FindNode('devnotes');
    if Assigned(LXmlDevNotes) then begin
      LXmlSummary := LXmlDevNotes.ChildNodes.FindNode('summary');
      if not (Assigned(LXmlSummary) and LXmlSummary.IsTextElement) then
        Continue;

      LDocStr.Name := LXmlMember.GetAttribute('name');
      LDocStr.&Type := LXmlMember.NodeName;
      LDocStr.DocStr := LXmlSummary.Text;
      LDocStr.Children := [];

      Result := Result + [LDocStr];
    end;
  end;
end;

procedure TPythonDocXml.DiscoverAndBufferize(const ADirectory: string);
var
  LFiles: TArray<string>;
  I: integer;
begin
  if not TDirectory.Exists(ADirectory) then
    Exit();

  LFiles := TDirectory.GetFiles(ADirectory,
    '*.xml',
    TSearchOption.soAllDirectories);

  if not Assigned(LFiles) then
    Exit();

  //Add all symbol docs discovered from a unit (currently classes only)
  for I := Low(LFiles) to High(LFiles) do
    FDiscoveredFiles.TryAdd(
      TPath.GetFileName(LFiles[I].Replace('.xml', '')), Bufferize(LFiles[I], ADirectory));
end;

function TPythonDocXml.LeanUp: IXMLDocument;

  procedure AddDoc(const AParent: IXMLNode; const AUnit: string; const ADocStr: TDocStr);
  begin
    var LNode := AParent.AddChild(ADocStr.&Type);
    if AParent.NodeName = 'docs' then
      LNode.Attributes['unit'] := AUnit;
    LNode.Attributes['name'] := ADocStr.Name;
    LNode.AddChild('docstr').Text := ADocStr.DocStr;

    if not Assigned(ADocStr.Children) then
      Exit;

    LNode := LNode.AddChild('members');
    for var LChild in ADocStr.Children do
      AddDoc(LNode, AUnit, LChild);
  end;

begin
  Result := NewXMLDocument();
  Result.Encoding := 'UTF-8';
  Result.Options := [doNodeAutoIndent];

  var LRootNode := Result.AddChild('docs');

  for var LDiscFile in FDiscoveredFiles do
    for var LDiscSym in LDiscFile.Value do
      AddDoc(LRootNode, LDiscFile.Key, LDiscSym);
end;

begin
  try
    DefaultDomVendor := sOmniXmlVendor;

    const INFO =
    '(*******************************************************' + sLineBreak +
    '*          XML Doc Compiler for P4D Modules            *' + sLineBreak +
    '*                                                      *' + sLineBreak +
    '*                                                      *' + sLineBreak +
    '* Generates a minimalistic XML doc as a result of a    *' + sLineBreak +
    '* group of XML doc files.                              *' + sLineBreak +
    '*                                                      *' + sLineBreak +
    '*                                                      *' + sLineBreak +
    '* For full license text and more information visit:    *' + sLineBreak +
    '* https://github.com/Embarcadero/python4delphi         *' + sLineBreak +
    '*******************************************************)';

    WriteLn(INFO + sLineBreak + sLineBreak);

    var LLean := TPythonDocXml.Create();
    try
      var LInput := String.Empty;
      while not TDirectory.Exists(LInput) do begin
        WriteLn('Type the XML files input dir: ');
        ReadLn(LInput);
      end;

      var LOutput := String.Empty;
      while not TDirectory.Exists(LOutput) do begin
        WriteLn('Type the XML file output dir: ');
        ReadLn(LOutput);
      end;

      LLean.DiscoverAndBufferize(LInput);
      var LXMLDoc := LLean.LeanUp();
      if Assigned(LXMLDoc) then
        LXMLDoc.SaveToFile(TPath.Combine(LOutput, 'docs.xml'));
    finally
      LLean.Free();
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
