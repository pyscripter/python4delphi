unit unUnitParser;

(**************************************************************************)
(*                                                                        *)
(* VCL Generator                       Copyright (c) 1998                 *)
(*                                                                        *)
(*                                     Morgan Martinet                    *)
(*                                     23 rue du 14 juillet               *)
(*                                     94270 le Kremlin-Bicetre           *)
(*                                     Phone (Work): 01 47 25 70 77       *)
(*                                     e-mail: mmm@imaginet.fr            *)
(*                                                                        *)
(**************************************************************************)
(* This source code is distributed with no WARRANTY, for no reason or use.*)
(* Everyone is allowed to use and change this code free for his own tasks *)
(* and projects, as long as this header and its copyright text is intact. *)
(* For changed versions of this code, which are public distributed the    *)
(* following additional conditions have to be fullfilled:                 *)
(* 1) The header has to contain a comment on the change and the author of *)
(*    it.                                                                 *)
(* 2) A copy of the changed source has to be sent to the above E-Mail     *)
(*    address or my then valid address, if this is possible to the        *)
(*    author.                                                             *)
(* The second condition has the target to maintain an up to date central  *)
(* version of the component. If this condition is not acceptable for      *)
(* confidential or legal reasons, everyone is free to derive a component  *)
(* or to generate a diff file to my or other original sources.            *)
(**************************************************************************)


interface
uses unParser, Classes, SysUtils, ComCtrls;

type
  TUnit = class;

  TPascalItem = class
    protected
      FName : String;
      FRefUnit : TUnit;

    public
      procedure AddToTree( tree : TTreeView; node : TTreeNode ); virtual; abstract;
      function AsString : String; virtual; abstract;
      function GetName : String; virtual;

      property Name : String read FName write FName;
      property RefUnit : TUnit read FRefUnit write FRefUnit;
  end;

  TVar = class(TPascalItem)
    protected
      FVarType : String;
      FItems : TStringList;
      FValue : String;

    public
      constructor Create;
      destructor  Destroy; override;
      procedure AddToTree( tree : TTreeView; node : TTreeNode ); override;
      function AsString : String; override;

      property VarType : String read FVarType write FVarType;
      property Items : TStringList read FItems;
      property Value : String read FValue write FValue;
  end;

  TConst = class(TPascalItem)
    protected
      FConstType : String;
      FConstValue : String;

    public
      procedure AddToTree( tree : TTreeView; node : TTreeNode ); override;
      function AsString : String; override;

      property ConstType : String read FConstType write FConstType;
      property ConstValue : String read FConstValue write FConstValue;
  end;

  TType = class(TPascalItem)
    public
      procedure AddToTree( tree : TTreeView; node : TTreeNode ); override;
      function AsString : String; override;
  end;

  TMyClass = class(TType)
    protected
      FPrivateList     : TList;
      FProtectedList   : TList;
      FPublicList      : TList;
      FPublishedList   : TList;
      FAutomatedList   : TList;
      FCurrentSection  : TList;
      FParentClasses   : TStringList;

      procedure AddSectionsToTree( tree : TTreeView; node : TTreeNode );

    public
      constructor Create;
      destructor  Destroy; override;

      procedure AddToTree( tree : TTreeView; node : TTreeNode ); override;
      function  AsString : String; override;
      procedure ParseSections( Parser : TMyParser; AUnit : TUnit );

      property PrivateList     : TList read FPrivateList;
      property ProtectedList   : TList read FProtectedList;
      property PublicList      : TList read FPublicList;
      property PublishedList   : TList read FPublishedList;
      property AutomatedList   : TList read FAutomatedList;
      property CurrentSection  : TList read FCurrentSection write FCurrentSection;
      property ParentClasses   : TStringList read FParentClasses;
  end;

  TClassRef = class(TType)
    public
      ClassRef : String;
      procedure AddToTree( tree : TTreeView; node : TTreeNode ); override;
      function AsString : String; override;
  end;

  TMyInterface = class(TMyClass)
    public
      Signature : String;
      procedure AddToTree( tree : TTreeView; node : TTreeNode ); override;
      function AsString : String; override;
  end;

  TMyDispInterface = class(TMyInterface)
    public
      procedure AddToTree( tree : TTreeView; node : TTreeNode ); override;
      function AsString : String; override;
  end;

  TEnum = class(TType)
    protected
      FItems : TStringList;

    public
      constructor Create;
      destructor  Destroy; override;

      procedure AddToTree( tree : TTreeView; node : TTreeNode ); override;
      function AsString : String; override;

      property Items : TStringList read FItems;
  end;

  TNewType = class(TType)
    public
      TypeRenamed  : String;
      IntervalFrom : String;
      IntervalTo   : String;

      procedure AddToTree( tree : TTreeView; node : TTreeNode ); override;
      function AsString : String; override;
  end;

  TPointer = class(TType)
    public
      PointerOn : String;

      procedure AddToTree( tree : TTreeView; node : TTreeNode ); override;
      function AsString : String; override;
  end;

  TRecord = class(TType)
    public
      Fields : TList;
      IsPacked : Boolean;
      constructor Create;
      destructor  Destroy; override;

      procedure AddToTree( tree : TTreeView; node : TTreeNode ); override;
      function AsString : String; override;
  end;

  TSet = class(TType)
    public
      TypeOfSet : TPascalItem;

      destructor  Destroy; override;
      procedure AddToTree( tree : TTreeView; node : TTreeNode ); override;
      function AsString : String; override;
  end;

  TArrayDim = class(TPascalItem)
    public
      Low : String;
      Hi : String;
      TypeInterval : String;

      procedure AddToTree( tree : TTreeView; node : TTreeNode ); override;
      function AsString : String; override;
  end;

  TArray = class(TType)
    protected
      FItems : TList;
    public
      TypeOfArray : String;
      IsPacked : Boolean;

      constructor Create;
      destructor  Destroy; override;
      procedure AddToTree( tree : TTreeView; node : TTreeNode ); override;
      function AsString : String; override;

      property Items : TList read FItems;
  end;

  TTypeFunc = class(TType)
    public
      Args        : TPascalItem;
      Modifiers   : TPascalItem;
      ReturnType  : String;
      IsOfObject  : Boolean;
      CallType    : String;

      destructor Destroy; override;
      procedure AddToTree( tree : TTreeView; node : TTreeNode ); override;
      function AsString : String; override;
  end;

  TTypeProc = class(TType)
    public
      Args        : TPascalItem;
      Modifiers   : TPascalItem;
      IsOfObject  : Boolean;
      CallType    : String;

      destructor Destroy; override;
      procedure AddToTree( tree : TTreeView; node : TTreeNode ); override;
      function AsString : String; override;
  end;

  TMethod = class(TPascalItem)
    public
      Args        : TPascalItem;
      Modifiers   : TPascalItem;
      IsClass     : Boolean;
      Count       : Integer;

      destructor Destroy; override;
      function GetName : String; override;
  end;

  TFunc = class(TMethod)
    public
      ReturnType  : String;

      destructor Destroy; override;
      procedure AddToTree( tree : TTreeView; node : TTreeNode ); override;
      function AsString : String; override;
  end;

  TProc = class(TMethod)
    public

      destructor Destroy; override;
      procedure AddToTree( tree : TTreeView; node : TTreeNode ); override;
      function AsString : String; override;
  end;

  TProperty = class(TPascalItem)
    public
      ReturnType     : String;
      PropRead       : String;
      PropWrite      : String;
      PropDefault    : String;
      PropStore      : String;
      PropDispId     : String;
      PropIndex      : String;
      PropImplements : String;
      IndexName      : String;
      IndexType      : String;
      IsIndexConst   : Boolean;
      IsReadOnly     : Boolean;
      Args           : TPascalItem;

      destructor Destroy; override;
      procedure AddToTree( tree : TTreeView; node : TTreeNode ); override;
      function AsString : String; override;
  end;

  TConstructor = class(TMethod)
    public

      destructor Destroy; override;
      procedure AddToTree( tree : TTreeView; node : TTreeNode ); override;
      function AsString : String; override;
  end;

  TDestructor = class(TPascalItem)
    public
      Args        : TPascalItem;
      Modifiers   : TPascalItem;

      destructor Destroy; override;
      procedure AddToTree( tree : TTreeView; node : TTreeNode ); override;
      function AsString : String; override;
  end;

  TArg = class(TPascalItem)
    protected
      FItems : TStringList;
    public
      IsConst : Boolean;
      IsVar : Boolean;
      IsOut : Boolean;
      IsArrayOf : Boolean;
      DefaultValue : String;

      constructor Create;
      destructor  Destroy; override;
      procedure AddToTree( tree : TTreeView; node : TTreeNode ); override;
      function AsString : String; override;
      property Items : TStringList read FItems;
  end;

  TArgs = class(TPascalItem)
    protected
      FItems : TList;
    public
      constructor Create;
      destructor  Destroy; override;

      procedure AddToTree( tree : TTreeView; node : TTreeNode ); override;
      function AsString : String; override;

      property Items : TList read FItems;
  end;

  TModifiers = class(TPascalItem)
    protected
      FItems : TStringList;
    public
      constructor Create;
      destructor  Destroy; override;

      procedure AddToTree( tree : TTreeView; node : TTreeNode ); override;
      function AsString : String; override;
      property Items : TStringList read FItems;
  end;

  TUnit = class(TPascalItem)
    protected
      FParser          : TMyParser;
      FUsesList        : TStringList;
      FVars            : TList;
      FConsts          : TList;
      FThreadVars      : TList;
      FTypes           : TList;
      FResourceStrings : TList;

      function GetVarCount : Integer;
      function GetVars( idx : Integer ) : TVar;
      function GetConstCount : Integer;
      function GetConsts( idx : Integer ) : TConst;
      function GetThreadVarCount : Integer;
      function GetThreadVars( idx : Integer ) : TVar;
      function GetTypeCount : Integer;
      function GetTypes( idx : Integer ) : TPascalItem;
      function GetResourceStringCount : Integer;
      function GetResourceStrings( idx : Integer ) : TPascalItem;

    public
      constructor Create;
      destructor  Destroy; override;

      procedure Parse( const str : String );
      procedure ParseVars;
      procedure ParseThreadVars;
      procedure ParseConsts;
      procedure ParseResourceStrings;
      procedure ParseTypes;
      function  ParseVar : TVar;
      function  ParseConst : TConst;
      function  ParseNewType( const TypeName : String ) : TNewType;
      function  ParseType : TPascalItem;
      function  ParseFunc : TFunc;
      function  ParseProc : TProc;
      function  ParseClass( const TypeName : String ) : TMyClass;
      function  ParseClassRef( const TypeName : String ) : TClassRef;
      function  ParseInterface( const TypeName : String ) : TMyInterface;
      function  ParseDispInterface( const TypeName : String ) : TMyDispInterface;
      function  ParseEnum( const TypeName : String ) : TEnum;
      function  ParsePointer( const TypeName : String ) : TPointer;
      function  ParseRecord( const TypeName : String; isPacked : Boolean ) : TRecord;
      function  ParseSet( const TypeName : String ) : TSet;
      function  ParseArray( const TypeName : String; isPacked : Boolean ) : TArray;
      function  ParseTypeFunc( const TypeName : String ) : TTypeFunc;
      function  ParseTypeProc( const TypeName : String ) : TTypeProc;
      function  ParseProperty : TProperty;
      function  ParseConstructor : TConstructor;
      function  ParseDestructor : TDestructor;
      function  ParseArgs : TArgs;
      function  ParseModifiers : TModifiers;
      function  IsSection : Boolean;
      function  IsCallType : Boolean;

      procedure AddToTree( tree : TTreeView; node : TTreeNode ); override;
      function AsString : String; override;

      property UsesList : TStringList read FUsesList write FUsesList;
      property Vars[idx : Integer ] : TVar read GetVars;
      property VarCount : Integer read GetVarCount;
      property Consts[idx : Integer ] : TConst read GetConsts;
      property ConstCount : Integer read GetConstCount;
      property ThreadVars[idx : Integer ] : TVar read GetThreadVars;
      property ThreadVarCount : Integer read GetThreadVarCount;
      property Types[idx : Integer ] : TPascalItem read GetTypes;
      property TypeCount : Integer read GetTypeCount;
      property ResourceStrings[idx : Integer] : TPascalItem read GetResourceStrings;
      property ResourceStringCount : Integer read GetResourceStringCount;
  end;

implementation

////////////////////////////////////////////////////
//
//  class TPascalItem
//
////////////////////////////////////////////////////

function TPascalItem.GetName : String;
begin
  Result := Name;
end;

////////////////////////////////////////////////////
//
//  class TEnum
//
////////////////////////////////////////////////////

constructor TEnum.Create;
begin
  inherited;
  FItems := TStringList.Create;
end;

destructor  TEnum.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TEnum.AddToTree( tree : TTreeView; node : TTreeNode );
var
  n : TTreeNode;
  i : Integer;
begin
  tree.Items.AddChild( node, AsString );
  exit;
  n := tree.Items.AddChild( node, Format('enum %s', [Name]) );
  for i := 0 to Items.Count - 1 do
    tree.Items.AddChild( n, Items.Strings[i] );
end;

function TEnum.AsString : String;
var
  i : Integer;
  s : String;
begin
  if Items.Count > 0 then
    begin
      s := '( ' + Items.Strings[0];
      for i := 1 to Items.Count - 1 do
        s := s + ', ' + Items.Strings[i];
      s := s + ' )';
    end
  else
    s := '';
  if Name <> '' then
    Result := Format('%s = %s;', [Name, s])
  else
    Result := s;
end;

////////////////////////////////////////////////////
//
//  class TMyClass
//
////////////////////////////////////////////////////

constructor TMyClass.Create;
begin
  inherited;
  FPrivateList     := TList.Create;
  FProtectedList   := TList.Create;
  FPublicList      := TList.Create;
  FPublishedList   := TList.Create;
  FAutomatedList   := TList.Create;
  CurrentSection   := FPublicList;
  FParentClasses   := TStringList.Create;
end;

destructor  TMyClass.Destroy;
begin
  FPrivateList.Free;
  FProtectedList.Free;
  FPublicList.Free;
  FPublishedList.Free;
  FAutomatedList.Free;
  FParentClasses.Free;
  inherited;
end;

procedure TMyClass.AddSectionsToTree( tree : TTreeView; node : TTreeNode );
var
  i : Integer;
  n : TTreeNode;
begin
  if PrivateList.Count > 0 then
    begin
      n := tree.Items.AddChild( node, 'Private' );
      for i := 0 to PrivateList.Count - 1 do
        tree.Items.AddChild( n, TPascalItem(PrivateList.Items[i]).AsString );
    end;
  if ProtectedList.Count > 0 then
    begin
      n := tree.Items.AddChild( node, 'Protected' );
      for i := 0 to ProtectedList.Count - 1 do
        tree.Items.AddChild( n, TPascalItem(ProtectedList.Items[i]).AsString );
    end;
  if PublicList.Count > 0 then
    begin
      n := tree.Items.AddChild( node, 'Public' );
      for i := 0 to PublicList.Count - 1 do
        tree.Items.AddChild( n, TPascalItem(PublicList.Items[i]).AsString );
    end;
  if PublishedList.Count > 0 then
    begin
      n := tree.Items.AddChild( node, 'Published' );
      for i := 0 to PublishedList.Count - 1 do
        tree.Items.AddChild( n, TPascalItem(PublishedList.Items[i]).AsString );
    end;
end;

procedure TMyClass.AddToTree( tree : TTreeView; node : TTreeNode );
var
  i : Integer;
  p : TTreeNode;
  s : String;
begin
  if ParentClasses.Count > 0 then
    begin
      s := ParentClasses.Strings[0];
      for i := 1 to ParentClasses.Count - 1 do
        s := s + ', ' + ParentClasses.Strings[i];
      s := Format( 'class %s( %s )', [Name, s]);
    end
  else
    s := Format( 'class %s', [Name]);
  p := tree.Items.AddChild( node, s );
  AddSectionsToTree( tree, p );
end;

function TMyClass.AsString : String;
begin
  Result := Format( 'class %s', [Name]);
end;

procedure TMyClass.ParseSections( Parser : TMyParser; AUnit : TUnit );

  function CountMeth( meth : TPascalItem ) : TPascalItem;
  var
    m : TMethod;
    i : Integer;
    item : TPascalItem;
  begin
    Result := meth;
    if not (meth is TMethod) then Exit;
    m := TMethod(meth);
    for i := CurrentSection.Count-1 downto 0 do
      begin
        item := TPascalItem( CurrentSection.Items[i] );
        if (item.ClassName = m.ClassName) and
           (m.Name = item.Name) then
          begin
            m.Count := (item as TMethod).Count + 1;
            Break;
          end;
      end;
  end;

var
  s : String;
  tmp : TPascalItem;
begin
  with Parser, AUnit do
    begin
      while (PeekChar <> ';') and
            (UpperCase( PeekSymbol ) <> 'END') do
        begin
          s := UpperCase(PeekSymbol);
          if s = 'PRIVATE' then
            begin
              ReadSymbol;
              CurrentSection := PrivateList
            end
          else if s = 'PROTECTED' then
            begin
              ReadSymbol;
              CurrentSection := ProtectedList;
            end
          else if s = 'PUBLIC' then
            begin
              ReadSymbol;
              CurrentSection := PublicList;
            end
          else if s = 'PUBLISHED' then
            begin
              ReadSymbol;
              CurrentSection := PublishedList;
            end
          else if s = 'AUTOMATED' then
            begin
              ReadSymbol;
              CurrentSection := AutomatedList;
            end
          else if s = 'CONSTRUCTOR' then
            CurrentSection.Add( CountMeth( ParseConstructor ) )
          else if s = 'DESTRUCTOR' then
            CurrentSection.Add( ParseDestructor )
          else if s = 'PROCEDURE' then
            CurrentSection.Add( CountMeth( ParseProc ) )
          else if s = 'FUNCTION' then
            CurrentSection.Add( CountMeth( ParseFunc ) )
          else if s = 'PROPERTY' then
            CurrentSection.Add( ParseProperty )
          else if s = 'CLASS' then
            begin
              ReadSymbol; // pass "class"
              SkipWhite;
              tmp := nil;
              if UpperCase( PeekSymbol ) = 'PROCEDURE' then
                tmp := ParseProc
              else if UpperCase( PeekSymbol ) = 'FUNCTION' then
                tmp := ParseFunc
              else
                RaiseErr('Could not find "procedure" or "function"');
              if Assigned(tmp) then
                begin
                  (tmp as TMethod).IsClass := True;
                  CurrentSection.Add( CountMeth( tmp ) );
                end;
            end
          else
            CurrentSection.Add( ParseVar );
          SkipWhite;
        end;
      if PeekChar <> ';' then
        ReadSymbol; // pass "end"
      SkipSemiColon;
    end;
end;


////////////////////////////////////////////////////
//
//  class TClassRef
//
////////////////////////////////////////////////////

procedure TClassRef.AddToTree( tree : TTreeView; node : TTreeNode );
begin
  tree.Items.AddChild( node, AsString );
end;

function TClassRef.AsString : String;
begin
  Result := Format('%s = class of %s;', [Name, ClassRef]);
end;


////////////////////////////////////////////////////
//
//  class TMyInterface
//
////////////////////////////////////////////////////

procedure TMyInterface.AddToTree( tree : TTreeView; node : TTreeNode );
var
  i : Integer;
  p : TTreeNode;
  s : String;
begin
  if ParentClasses.Count > 0 then
    begin
      s := ParentClasses.Strings[0];
      for i := 1 to ParentClasses.Count - 1 do
        s := s + ', ' + ParentClasses.Strings[i];
      s := Format( 'interface %s( %s )', [Name, s]);
    end
  else
    s := Format( 'interface %s', [Name]);
  p := tree.Items.AddChild( node, s );
  tree.Items.AddChild( p, 'Signature = '+Signature );
  AddSectionsToTree( tree, p );
end;

function TMyInterface.AsString : String;
begin
  Result := Format( 'interface %s', [Name]);
end;


////////////////////////////////////////////////////
//
//  class TMyDispInterface
//
////////////////////////////////////////////////////

procedure TMyDispInterface.AddToTree( tree : TTreeView; node : TTreeNode );
var
  i : Integer;
  p : TTreeNode;
  s : String;
begin
  if ParentClasses.Count > 0 then
    begin
      s := ParentClasses.Strings[0];
      for i := 1 to ParentClasses.Count - 1 do
        s := s + ', ' + ParentClasses.Strings[i];
      s := Format( 'dispinterface %s( %s )', [Name, s]);
    end
  else
    s := Format( 'dispinterface %s', [Name]);
  p := tree.Items.AddChild( node, s );
  tree.Items.AddChild( p, 'Signature = '+Signature );
  AddSectionsToTree( tree, p );
end;

function TMyDispInterface.AsString : String;
begin
  Result := Format( 'dispinterface %s', [Name]);
end;


////////////////////////////////////////////////////
//
//  class TVar
//
////////////////////////////////////////////////////

constructor TVar.Create;
begin
  inherited;
  FItems := TStringList.Create;
end;

destructor  TVar.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TVar.AddToTree( tree : TTreeView; node : TTreeNode );
begin
  tree.Items.AddChild( node, AsString );
end;

function TVar.AsString : String;
var
  i : Integer;
begin
  Result := '';
  if Items.Count = 0 then
    Exit;
  Result := Items.Strings[0];
  for i := 1 to Items.Count - 1 do
    Result := Result + ', ' + Items.Strings[i];
  Result := Format('%s : %s', [Result, VarType]);
  if Value <> '' then
    Result := Result + ' = ' + Value;
  Result := Result + ';';
end;


////////////////////////////////////////////////////
//
//  class TConst
//
////////////////////////////////////////////////////

procedure TConst.AddToTree( tree : TTreeView; node : TTreeNode );
begin
  tree.Items.AddChild( node, AsString );
end;

function TConst.AsString : String;
begin
  if ConstType <> '' then
    Result := Format('%s : %s = %s', [Name, ConstType, ConstValue])
  else
    Result := Format('%s = %s', [Name, ConstValue]);
  Result := Result + ';';
end;


////////////////////////////////////////////////////
//
//  class TType
//
////////////////////////////////////////////////////

procedure TType.AddToTree( tree : TTreeView; node : TTreeNode );
begin
end;

function TType.AsString : String;
begin
  Result := '';
end;


////////////////////////////////////////////////////
//
//  class TNewType
//
////////////////////////////////////////////////////

procedure TNewType.AddToTree( tree : TTreeView; node : TTreeNode );
begin
  tree.Items.AddChild( node, AsString );
end;

function TNewType.AsString : String;
begin
  if IntervalFrom <> '' then
    Result := Format('%s..%s', [IntervalFrom, IntervalTo])
  else
    Result := TypeRenamed;
  if Name <> '' then
    Result := Name + ' = ' + Result + ';';
end;


////////////////////////////////////////////////////
//
//  class TPointer
//
////////////////////////////////////////////////////

procedure TPointer.AddToTree( tree : TTreeView; node : TTreeNode );
begin
  tree.Items.AddChild( node, AsString );
end;

function TPointer.AsString : String;
begin
  Result := Format('%s = ^%s;', [Name, PointerOn]);
end;


////////////////////////////////////////////////////
//
//  class TRecord
//
////////////////////////////////////////////////////

constructor TRecord.Create;
begin
  inherited;
  Fields := TList.Create;
end;

destructor  TRecord.Destroy;
begin
  Fields.Free;
  inherited;
end;

procedure TRecord.AddToTree( tree : TTreeView; node : TTreeNode );
var
  n : TTreeNode;
  i : Integer;
begin
  n := tree.Items.AddChild( node, Format('record %s', [Name]) );
  for i := 0 to Fields.Count - 1 do
    TPascalItem(Fields.Items[i]).AddToTree( tree, n );
end;

function TRecord.AsString : String;
begin
  Result := Format('record %s', [Name]);
end;


////////////////////////////////////////////////////
//
//  class TSet
//
////////////////////////////////////////////////////

destructor  TSet.Destroy;
begin
  TypeOfSet.Free;
  inherited;
end;

procedure TSet.AddToTree( tree : TTreeView; node : TTreeNode );
var
  n : TTreeNode;
begin
  tree.Items.AddChild( node, AsString );
  exit;
  n := tree.Items.AddChild( node, Format('set %s of', [Name]) );
  if Assigned(TypeOfSet) then
    TypeOfSet.AddToTree( tree, n );
end;

function TSet.AsString : String;
begin
  Result := Format('%s = set of %s;', [Name, TypeOfSet.AsString]);
end;


////////////////////////////////////////////////////
//
//  class TArrayDim
//
////////////////////////////////////////////////////

procedure TArrayDim.AddToTree( tree : TTreeView; node : TTreeNode );
begin
  tree.Items.AddChild( node, AsString );
end;

function TArrayDim.AsString : String;
begin
  if TypeInterval <> '' then
    Result := Format('%s', [TypeInterval])
  else
    Result := Format('%s..%s', [Low, Hi]);
end;


////////////////////////////////////////////////////
//
//  class TArray
//
////////////////////////////////////////////////////

constructor TArray.Create;
begin
  inherited;
  FItems := TList.Create;
end;

destructor  TArray.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TArray.AddToTree( tree : TTreeView; node : TTreeNode );
var
  i : Integer;
  n : TTreeNode;
begin
  tree.Items.AddChild( node, AsString );
  exit;
  n := tree.Items.AddChild( node, Format('array %s of %s', [Name, TypeOfArray]) );
  for i := 0 to Items.Count - 1 do
    TPascalItem(Items.Items[i]).AddToTree( tree, n );
end;

function TArray.AsString : String;
var
  i : Integer;
  s : String;
begin
  if Items.Count > 0 then
    begin
      s := '[ ' + TPascalItem(Items.Items[0]).AsString;
      for i := 1 to Items.Count - 1 do
        s := s + ', ' + TPascalItem(Items.Items[i]).AsString;
      s := s + ' ]';
    end
  else
    s := '';
  Result := Format('%s = array %s of %s', [Name, s, TypeOfArray]);
end;


////////////////////////////////////////////////////
//
//  class TTypeFunc
//
////////////////////////////////////////////////////

destructor TTypeFunc.Destroy;
begin
  inherited;
  Args.Free;
  Modifiers.Free;
end;

procedure TTypeFunc.AddToTree( tree : TTreeView; node : TTreeNode );
var
  p : TTreeNode;
  s : String;
begin
  tree.Items.AddChild( node, AsString );
  exit;
  s := Format('%s = function : %s %s', [Name, ReturnType, CallType]);
  s := Trim(s);
  if IsOfObject then
    s := s + ' of object';
  p := tree.Items.AddChild( node, s );
  Args.AddToTree( tree, p );
end;

function TTypeFunc.AsString : String;
var
  s : String;
begin
  s := Format('%s = function%s : %s %s', [Name, Args.AsString, ReturnType, CallType]);
  s := Trim(s);
  if IsOfObject then
    s := s + ' of object';
  Result := s+';';
end;


////////////////////////////////////////////////////
//
//  class TTypeProc
//
////////////////////////////////////////////////////

destructor TTypeProc.Destroy;
begin
  inherited;
  Args.Free;
  Modifiers.Free;
end;

procedure TTypeProc.AddToTree( tree : TTreeView; node : TTreeNode );
var
  p : TTreeNode;
  s : String;
begin
  tree.Items.AddChild( node, AsString );
  exit;
  s := Format('%s = procedure %s', [Name, CallType]);
  s := Trim(s);
  if IsOfObject then
    s := s + ' of object';
  p := tree.Items.AddChild( node, s );
  Args.AddToTree( tree, p );
end;

function TTypeProc.AsString : String;
var
  s : String;
begin
  s := Format('%s = procedure%s %s', [Name, Args.AsString, CallType]);
  s := Trim(s);
  if IsOfObject then
    s := s + ' of object';
  Result := s+';';
end;


////////////////////////////////////////////////////
//
//  class TMethod
//
////////////////////////////////////////////////////

destructor TMethod.Destroy;
begin
  inherited;
  Args.Free;
  Modifiers.Free;
end;

function TMethod.GetName : String;
begin
  Result := Name;
  if Count > 0 then
    Result := Result + IntToStr(Count);
end;

////////////////////////////////////////////////////
//
//  class TFunc
//
////////////////////////////////////////////////////

destructor TFunc.Destroy;
begin
  inherited;
end;

procedure TFunc.AddToTree( tree : TTreeView; node : TTreeNode );
var
  p : TTreeNode;
  s : String;
begin
  tree.Items.AddChild( node, AsString );
  exit;
  s := '';
  if IsClass then
    s := 'class ';
  s := s + 'function';
  p := tree.Items.AddChild( node, Format('%s %s : %s', [s, Name, ReturnType]) );
  Args.AddToTree( tree, p );
  Modifiers.AddToTree( tree, p );
end;

function TFunc.AsString : String;
var
  s : String;
begin
  s := '';
  if IsClass then
    s := 'class ';
  s := s + 'function';
  Result := Format('%s %s%s : %s; %s', [s, Name, Args.AsString, ReturnType, Modifiers.AsString]);
end;


////////////////////////////////////////////////////
//
//  class TProc
//
////////////////////////////////////////////////////

destructor TProc.Destroy;
begin
  inherited;
end;

procedure TProc.AddToTree( tree : TTreeView; node : TTreeNode );
var
  p : TTreeNode;
  s : String;
begin
  tree.Items.AddChild( node, AsString );
  exit;
  s := '';
  if IsClass then
    s := 'class ';
  s := s + 'procedure';
  p := tree.Items.AddChild( node, Format('%s %s', [s, Name]) );
  Args.AddToTree( tree, p );
  Modifiers.AddToTree( tree, p );
end;

function TProc.AsString : String;
var
  s : String;
begin
  s := '';
  if IsClass then
    s := 'class ';
  s := s + 'procedure';
  Result := Format('%s %s%s; %s', [s, Name, Args.AsString, Modifiers.AsString]);
end;


////////////////////////////////////////////////////
//
//  class TProperty
//
////////////////////////////////////////////////////

destructor TProperty.Destroy;
begin
  Args.Free;
  inherited;
end;

procedure TProperty.AddToTree( tree : TTreeView; node : TTreeNode );
begin
  tree.Items.AddChild( node, AsString )
end;

function TProperty.AsString : String;
begin
  Result := 'property ' + Name;
  if Assigned(Args) then
    Result := Result + '[' + Args.AsString + ']';
  if ReturnType <> '' then
    Result := Result + ' : ' + ReturnType;
  if PropIndex <> '' then
    Result := Result + ' index ' + PropIndex;
  if PropRead <> '' then
    Result := Result + ' read ' + PropRead;
  if PropWrite <> '' then
    Result := Result + ' write ' + PropWrite;
  if PropDefault <> '' then
    Result := Result + ' default ' + PropDefault;
  if PropStore <> '' then
    Result := Result + ' store ' + PropStore;
  if IsReadOnly then
    Result := Result + ' readonly';
  if PropDispId <> '' then
    Result := Result + ' dispid ' + PropDispId;
  Result := Result + ';';
end;


////////////////////////////////////////////////////
//
//  class TConstructor
//
////////////////////////////////////////////////////

destructor TConstructor.Destroy;
begin
  inherited;
end;

procedure TConstructor.AddToTree( tree : TTreeView; node : TTreeNode );
var
  p : TTreeNode;
begin
  tree.Items.AddChild( node, AsString );
  exit;
  p := tree.Items.AddChild( node, Format('constructor %s', [Name]) );
  Args.AddToTree( tree, p );
  Modifiers.AddToTree( tree, p );
end;

function TConstructor.AsString : String;
begin
  Result := Format('constructor %s%s; %s', [Name, Args.AsString, Modifiers.AsString]);
end;


////////////////////////////////////////////////////
//
//  class TDestructor
//
////////////////////////////////////////////////////

destructor TDestructor.Destroy;
begin
  inherited;
  Args.Free;
  Modifiers.Free;
end;

procedure TDestructor.AddToTree( tree : TTreeView; node : TTreeNode );
var
  p : TTreeNode;
begin
  tree.Items.AddChild( node, AsString );
  exit;
  p := tree.Items.AddChild( node, Format('destructor %s', [Name]) );
  Args.AddToTree( tree, p );
  Modifiers.AddToTree( tree, p );
end;

function TDestructor.AsString : String;
begin
  Result := Format('destructor %s%s; %s', [Name, Args.AsString, Modifiers.AsString]);
end;


////////////////////////////////////////////////////
//
//  class TArg
//
////////////////////////////////////////////////////

constructor TArg.Create;
begin
  inherited;
  FItems := TStringList.Create;
end;

destructor  TArg.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TArg.AddToTree( tree : TTreeView; node : TTreeNode );
begin
  tree.Items.AddChild( node, AsString );
end;

function TArg.AsString : String;
var
  i : Integer;
begin
  if IsConst then
    Result := 'const '
  else if IsVar then
    Result := 'var '
  else if IsOut then
    Result := 'out '
  else
    Result := '';
  if Items.Count > 0 then
    Result := Result + Items.Strings[0];
  for i := 1 to Items.Count - 1 do
    Result := Result + ', ' + Items.Strings[i];
  if Name <> '' then
    begin
      Result := Result + ' : ';
      if IsArrayOf then
        Result := Result + 'array of ';
      Result := Result + Name;
    end;
  if DefaultValue <> '' then
    Result := Result + ' = ' + DefaultValue;
end;


////////////////////////////////////////////////////
//
//  class TArgs
//
////////////////////////////////////////////////////

constructor TArgs.Create;
begin
  inherited;
  FItems := TList.Create;
end;

destructor  TArgs.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TArgs.AddToTree( tree : TTreeView; node : TTreeNode );
var
  n : TTreeNode;
  i : Integer;
begin
  if FItems.Count = 0 then
    Exit;
  n := tree.Items.AddChild( node, 'args' );
  for i := 0 to FItems.Count - 1 do
    TPascalItem(FItems.Items[i]).AddToTree( tree, n );
end;

function TArgs.AsString : String;
var
  i : Integer;
begin
  Result := '';
  if FItems.Count = 0 then
    Exit;
  Result := '( ' + TPascalItem(FItems.Items[0]).AsString;
  for i := 1 to FItems.Count - 1 do
    Result := Result + '; ' + TPascalItem(FItems.Items[i]).AsString;
  Result := Result + ' )';
end;


////////////////////////////////////////////////////
//
//  class TModifiers
//
////////////////////////////////////////////////////

constructor TModifiers.Create;
begin
  inherited;
  FItems := TStringList.Create;
end;

destructor  TModifiers.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TModifiers.AddToTree( tree : TTreeView; node : TTreeNode );
var
  n : TTreeNode;
  i : Integer;
begin
  if FItems.Count = 0 then
    Exit;
  n := tree.Items.AddChild( node, 'modifiers' );
  for i := 0 to FItems.Count - 1 do
    tree.Items.AddChild( n, FItems.Strings[i] );
    //TPascalItem(FItems.Items[i]).AddToTree( tree, n );
end;

function TModifiers.AsString : String;
var
  i : Integer;
begin
  Result := '';
  if FItems.Count = 0 then
    Exit;
  for i := 0 to FItems.Count - 1 do
    Result := Result + FItems.Strings[i] + '; ';
  Result := Trim( Result );
end;


////////////////////////////////////////////////////
//
//  class TUnit
//
////////////////////////////////////////////////////

function TUnit.GetVarCount : Integer;
begin
  Result := FVars.Count;
end;

function TUnit.GetVars( idx : Integer ) : TVar;
begin
  Result := TVar(FVars.Items[idx]);
end;

function TUnit.GetConstCount : Integer;
begin
  Result := FConsts.Count;
end;

function TUnit.GetConsts( idx : Integer ) : TConst;
begin
  Result := TConst(FConsts.Items[idx]);
end;

function TUnit.GetThreadVarCount : Integer;
begin
  Result := FThreadVars.Count;
end;

function TUnit.GetThreadVars( idx : Integer ) : TVar;
begin
  Result := TVar(FThreadVars.Items[idx]);
end;

function TUnit.GetTypeCount : Integer;
begin
  Result := FTypes.Count;
end;

function TUnit.GetTypes( idx : Integer ) : TPascalItem;
begin
  Result := TPascalItem(FTypes.Items[idx]);
end;

function TUnit.GetResourceStringCount : Integer;
begin
  Result := FResourceStrings.Count;
end;

function TUnit.GetResourceStrings( idx : Integer ) : TPascalItem;
begin
  Result := TPascalItem(FResourceStrings.Items[idx]);
end;

constructor TUnit.Create;
begin
  FParser           := TMyParser.Create;
  FUsesList         := TStringList.Create;
  FVars             := TList.Create;
  FConsts           := TList.Create;
  FThreadVars       := TList.Create;
  FTypes            := TList.Create;
  FResourceStrings  := TList.Create;
end;

destructor  TUnit.Destroy;
var
  i : Integer;
begin
  for i := 0 to VarCount - 1 do
    Vars[i].Free;
  FVars.Free;
  for i := 0 to ConstCount - 1 do
    Consts[i].Free;
  FConsts.Free;
  for i := 0 to ThreadVarCount - 1 do
    ThreadVars[i].Free;
  FThreadVars.Free;
  for i := 0 to TypeCount - 1 do
    Types[i].Free;
  FTypes.Free;
  for i := 0 to ResourceStringCount - 1 do
    ResourceStrings[i].Free;
  FResourceStrings.Free;
  FUsesList.Free;
  FParser.Free;
  inherited;
end;

procedure TUnit.Parse( const str : String );
var
  s : String;
begin
  with FParser do
    begin
      Text := str;
      SkipWhite;
      // Read unit
      s := UpperCase( ReadSymbol );
      if s <> 'UNIT' then
        RaiseErr('This unit does not start with the keyword "unit"');
      SkipWhite;
      // Read unit's name
      s := ReadSymbol;
      Name := s;
      SkipSemiColon;
      // Read interface
      s := UpperCase( ReadSymbol );
      if s <> 'INTERFACE' then
        RaiseErr('This unit does not contain the keyword "interface"');
      SkipWhite;
      // Read an optional uses instruction
      s := UpperCase( PeekSymbol );
      if s = 'USES' then
        begin
          ReadSymbol; // pass the "uses"
          SkipWhite;
          while PeekChar <> ';' do
            begin
              SkipWhite;
              s := ReadSymbol;
              FUsesList.Add( s );
              SkipWhite;
              if PeekChar = ',' then
                begin
                  ReadChar;
                  SkipWhite;
                end;
            end;
          ReadChar;
          SkipWhite;
        end;
      // Read interface sections
      s := UpperCase(PeekSymbol);
      while s <> 'IMPLEMENTATION' do
        begin
          if s = 'TYPE' then
            begin
              ReadSymbol;
              ParseTypes;
            end
          else if s = 'CONST' then
            begin
              ReadSymbol;
              ParseConsts;
            end
          else if s = 'VAR' then
            begin
              ReadSymbol;
              ParseVars;
            end
          else if s = 'THREADVAR' then
            begin
              ReadSymbol;
              ParseThreadVars;
            end
          else if s = 'RESOURCESTRING' then
            begin
              ReadSymbol;
              ParseResourceStrings;
            end
          else
            ParseTypes;
          s := UpperCase(PeekSymbol);
        end;
    end;
end;

procedure TUnit.ParseVars;
var
  s : String;
begin
  FParser.SkipWhite;
  repeat
    s := UpperCase( FParser.PeekSymbol );
    if (s = 'PROCEDURE') or (s = 'FUNCTION') then
      FTypes.Add( ParseType )
    else
      FVars.Add( ParseVar );
  until IsSection;
end;

procedure TUnit.ParseThreadVars;
var
  s : String;
begin
  FParser.SkipWhite;
  repeat
    s := UpperCase( FParser.PeekSymbol );
    if (s = 'PROCEDURE') or (s = 'FUNCTION') then
      FTypes.Add( ParseType )
    else
      FThreadVars.Add( ParseVar );
  until IsSection;
end;

procedure TUnit.ParseConsts;
var
  s : String;
begin
  FParser.SkipWhite;
  repeat
    s := UpperCase( FParser.PeekSymbol );
    if (s = 'PROCEDURE') or (s = 'FUNCTION') then
      FTypes.Add( ParseType )
    else
      FConsts.Add( ParseConst );
  until IsSection;
end;

procedure TUnit.ParseResourceStrings;
begin
  FParser.SkipWhite;
  repeat
    FResourceStrings.Add( ParseConst );
  until IsSection;
end;

procedure TUnit.ParseTypes;
var
  T : TPascalItem;
begin
  FParser.SkipWhite;
  repeat
    T := ParseType;
    if Assigned(T) then
      begin
        T.RefUnit := Self;
        FTypes.Add( T );
      end;
  until IsSection;
end;

// XXX : YYY;
function TUnit.ParseVar : TVar;
var
  tmp : TPascalItem;
begin
  with FParser do
    begin
      Result := TVar.Create;
      try
        while PeekChar <> ':' do
          begin
            Result.Items.Add( ReadSymbol );
            SkipWhite;
            if PeekChar = ',' then
              begin
                ReadChar; // pass the ','
                SkipWhite;
              end;
          end;
        ReadChar; // pass the ':'
        SkipWhite;
        repeat
          Result.VarType := Result.VarType + ReadUntil(['=', ';']);
          if (PeekChar = ';') and (ParenthesisDepth > 0) then
            begin
              ReadChar; // pass the ;
              SkipWhite;
            end;
        until ParenthesisDepth = 0;
        Result.VarType := Trim( Result.VarType );
        if PeekChar = '=' then
          begin
            ReadChar; // Skip the '='
            SkipWhite;
            Result.Value := Trim( ReadUntil([';']) );
          end;
        SkipSemiColon;
        tmp := ParseModifiers;
        tmp.Free;
      except
        Result.Free;
        raise;
      end;
    end;
end;

// XXX [: YYY] = ZZZ;
function TUnit.ParseConst : TConst;
var
  sName, sType, sValue : String;
begin
  with FParser do
    begin
      sName := ReadSymbol;
      SkipWhite;
      if PeekChar = ':' then
        begin
          ReadChar;
          SkipWhite;
          sType := Trim( ReadUntil( ['='] ) );
          SkipWhite;
        end;
      if PeekChar <> '=' then
        RaiseErr('could not find a "=" while parsing a const');
      ReadChar;
      SkipWhite;
      sValue := '';
      repeat
        sValue := sValue + ReadUntil( [';'] );
        if ParenthesisDepth > 0 then
          begin
            sValue := sValue + ';';
            ReadChar; // pass the ;
          end;
      until ParenthesisDepth = 0;
      SkipSemiColon;
      Result := TConst.Create;
      Result.Name  := sName;
      Result.ConstType  := sType;
      Result.ConstValue := sValue;
    end;
end;

function TUnit.ParseNewType( const TypeName : String ) : TNewType;
begin
  Result := TNewType.Create;
  try
    with FParser do
      begin
        Result.Name := TypeName;
        Result.TypeRenamed := ReadExpr;
        if PeekChar = '.' then
          begin
            ReadChar; // pass the '.'
            if ReadChar <> '.' then
              RaiseErr('no ".." in an interval');
            SkipWhite;
            Result.IntervalFrom := Result.TypeRenamed;
            Result.TypeRenamed := '';
            Result.IntervalTo := ReadExpr;
          end;
        SkipSemiColon;
      end;
  except
    Result.Free;
    raise;
  end;
end;

function TUnit.ParseType : TPascalItem;
var
  s, sTypeName, sType : String;
begin
  Result := nil;
  with FParser do
    begin
      s := UpperCase(PeekSymbol);
      if s = 'PROCEDURE' then
        begin
          Result := ParseProc;
          Exit;
        end
      else if s = 'FUNCTION' then
        begin
          Result := ParseFunc;
          Exit;
        end;
      sTypeName := ReadSymbol;
      SkipWhite;
      if ReadChar <> '=' then
        RaiseErr('Could not find "=" in a type declaration');
      SkipWhite;
      if PeekChar = '(' then
        begin
          Result := ParseEnum( sTypeName );
          Exit;
        end
      else if PeekChar = '^' then
        begin
          Result := ParsePointer( sTypeName );
          Exit;
        end;
      sType := UpperCase( PeekSymbol );
      if sType = 'CLASS' then
        begin
          ReadSymbol;
          SkipWhite;
          // we skip a forward declaration of a class
          if PeekChar = ';' then
            SkipSemiColon
          else if PeekChar ='(' then
            Result := ParseClass( sTypeName )
          else if UpperCase( PeekSymbol ) = 'OF' then
            Result := ParseClassRef( sTypeName )
          else
            Result := ParseClass( sTypeName );
        end
      else if sType = 'INTERFACE' then
        begin
          ReadSymbol;
          SkipWhite;
          // we skip a forward declaration of a class
          if PeekChar = ';' then
            SkipSemiColon
          else
            Result := ParseInterface( sTypeName );
        end
      else if sType = 'DISPINTERFACE' then
        begin
          ReadSymbol;
          SkipWhite;
          // we skip a forward declaration of a class
          if PeekChar = ';' then
            SkipSemiColon
          else
            Result := ParseDispInterface( sTypeName );
        end
      else if (sType = 'RECORD') then
        Result := ParseRecord( sTypeName, False )
      else if (sType = 'PACKED') then
        begin
          ReadSymbol;
          SkipWhite;
          sType := UpperCase(PeekSymbol);
          if sType = 'RECORD' then
            Result := ParseRecord( sTypeName, True )
          else
            Result := ParseArray( sTypeName, True );
        end
      else if sType = 'SET' then
        Result := ParseSet( sTypeName )
      else if sType = 'ARRAY' then
        Result := ParseArray( sTypeName, False )
      else if sType = 'PROCEDURE' then
        Result := ParseTypeProc( sTypeName )
      else if sType = 'FUNCTION' then
        Result := ParseTypeFunc( sTypeName )
      else
        Result := ParseNewType( sTypeName );
    end;
end;

// function XXX(arg1, arg2...argn) : YYY; [cdecl; external...];
function TUnit.ParseFunc : TFunc;
begin
  with FParser do
    begin
      Result := TFunc.Create;
      try
        if UpperCase(ReadSymbol) <> 'FUNCTION' then
          RaiseErr('Could not find "function"');
        SkipWhite;
        Result.Name := ReadSymbol;
        SkipWhite;
        if PeekChar = '.' then
          begin
            ReadChar; // pass the '.'
            ReadSymbol;
            SkipWhite;
            if ReadChar <> '=' then
              RaiseErr( 'Could not find "="' );
            SkipWhite;
            ReadSymbol;
            Result.Args := TArgs.Create;
          end
        else
          begin
            Result.Args := ParseArgs;
            if ReadChar <> ':' then
              RaiseErr('Could not find ":" when parsing a function');
            SkipWhite;
            Result.ReturnType := ReadSymbol;
          end;
        SkipWhite;
        if IsCallType then
          begin
            Result.Modifiers := TModifiers.Create;
            while IsCallType do
              begin
                TModifiers(Result.Modifiers).Items.Add( ReadSymbol );
                SkipWhite;
              end;
          end;
        SkipSemiColon;
        if not Assigned( Result.Modifiers ) then
          Result.Modifiers := ParseModifiers;
      except
        Result.Free;
        raise;
      end;
    end;
end;

// procedure XXX(arg1, arg2...argn); [cdecl; external...];
function TUnit.ParseProc : TProc;
begin
  with FParser do
    begin
      Result := TProc.Create;
      try
        if UpperCase(ReadSymbol) <> 'PROCEDURE' then
          RaiseErr('Could not find "procedure"');
        SkipWhite;
        Result.Name := ReadSymbol;
        SkipWhite;
        if PeekChar = '.' then
          begin
            ReadChar; // pass the '.'
            ReadSymbol;
            SkipWhite;
            if ReadChar <> '=' then
              RaiseErr( 'Could not find "="' );
            SkipWhite;
            ReadSymbol;
            Result.Args := TArgs.Create;
          end
        else
          Result.Args := ParseArgs;
        if IsCallType then
          begin
            Result.Modifiers := TModifiers.Create;
            while IsCallType do
              begin
                TModifiers(Result.Modifiers).Items.Add( ReadSymbol );
                SkipWhite;
              end;
          end;
        SkipSemiColon;
        if not Assigned( Result.Modifiers ) then
          Result.Modifiers := ParseModifiers;
      except
        Result.Free;
        raise;
      end;
    end;
end;

function TUnit.ParseClass( const TypeName : String ) : TMyClass;
begin
  with FParser do
    begin
      Result := TMyClass.Create;
      try
        Result.Name := TypeName;
        if PeekChar = '(' then
          begin
            ReadChar; // pass the (
            SkipWhite;
            while PeekChar <> ')' do
              begin
                Result.ParentClasses.Add( ReadSymbol );
                SkipWhite;
                if PeekChar = ',' then
                  begin
                    ReadChar; // pass ','
                    SkipWhite;
                  end;
              end;
            ReadChar; // pass ')'
            SkipWhite;
          end;
        Result.ParseSections( FParser, Self );
      except
        Result.Free;
        raise;
      end;
    end;
end;

// XXX = class of YYY
function  TUnit.ParseClassRef( const TypeName : String ) : TClassRef;
begin
  with FParser do
    begin
      Result := TClassRef.Create;
      try
        Result.Name := TypeName;
        SkipWhite;
        if UpperCase( ReadSymbol ) <> 'OF' then
          RaiseErr('could not find "of" while parsing a set');
        SkipWhite;
        Result.ClassRef := ReadSymbol;
        SkipSemiColon;
      except
        Result.Free;
        raise;
      end;
    end;
end;

function TUnit.ParseInterface( const TypeName : String ) : TMyInterface;
begin
  with FParser do
    begin
      Result := TMyInterface.Create;
      try
        Result.Name := TypeName;
        if PeekChar = '(' then
          begin
            ReadChar; // pass the (
            SkipWhite;
            while PeekChar <> ')' do
              begin
                Result.ParentClasses.Add( ReadSymbol );
                SkipWhite;
                if PeekChar = ',' then
                  begin
                    ReadChar; // pass ','
                    SkipWhite;
                  end;
              end;
            ReadChar; // pass ')'
            SkipWhite;
          end;
        if PeekChar = '[' then
          begin
            ReadChar; // pass the '['
            SkipWhite;
            Result.Signature := ReadExpr;
            if PeekChar <> ']' then
              RaiseErr('Could not find a matching "]"');
            ReadChar; // pass the ']'
            SkipWhite;
          end;
        Result.ParseSections( FParser, Self );
      except
        Result.Free;
        raise;
      end;
    end;
end;

function TUnit.ParseDispInterface( const TypeName : String ) : TMyDispInterface;
begin
  with FParser do
    begin
      Result := TMyDispInterface.Create;
      try
        Result.Name := TypeName;
        if PeekChar = '(' then
          begin
            ReadChar; // pass the (
            SkipWhite;
            while PeekChar <> ')' do
              begin
                Result.ParentClasses.Add( ReadSymbol );
                SkipWhite;
                if PeekChar = ',' then
                  begin
                    ReadChar; // pass ','
                    SkipWhite;
                  end;
              end;
            ReadChar; // pass ')'
            SkipWhite;
          end;
        if PeekChar = '[' then
          begin
            ReadChar; // pass the '['
            SkipWhite;
            Result.Signature := ReadExpr;
            if PeekChar <> ']' then
              RaiseErr('Could not find a matching "]"');
            ReadChar; // pass the ']'
            SkipWhite;
          end;
        Result.ParseSections( FParser, Self );
      except
        Result.Free;
        raise;
      end;
    end;
end;


// XXX = (a1, a2...an);
function TUnit.ParseEnum( const TypeName : String ) : TEnum;
var
  s : String;
begin
  with FParser do
    begin
      ReadChar; // pass the (
      SkipWhite;
      Result := TEnum.Create;
      try
        Result.Name := TypeName;
        while PeekChar <> ')' do
          begin
            s := ReadSymbol;
            Result.Items.Add( s );
            SkipWhite;
            if PeekChar = ',' then
              begin
                ReadChar;
                SkipWhite;
              end;
          end;
        ReadChar; // pass the ')'
        SkipSemiColon;
      except
        Result.Free;
        raise;
      end;
    end;
end;

//  XXX = ^YYY;
function TUnit.ParsePointer( const TypeName : String ) : TPointer;
begin
  with FParser do
    begin
      Result := TPointer.Create;
      try
        Result.Name := TypeName;
        if ReadChar <> '^' then
          RaiseErr( 'Could not find a "^" while parsing a pointer type' );
        SkipWhite;
        Result.PointerOn := ReadSymbol;
        SkipSemiColon;
      except
        Result.Free;
        raise;
      end;
    end;
end;

function TUnit.ParseRecord( const TypeName : String; isPacked : Boolean ) : TRecord;
  procedure ParseCase;
  begin
    with FParser do
      begin
        while not ((UpperCase(PeekChar) = 'E') and
                   (UpperCase(PeekSymbol) = 'END')) do
          if ReadChar = '{' then
            ReadUntil( ['}'] );
      end;
  end;
begin
  with FParser do
    begin
      Result := TRecord.Create;
      try
        Result.Name := TypeName;
        if isPacked then
          begin
            Result.IsPacked := True;
            SkipWhite;
            if UpperCase(ReadSymbol) <> 'RECORD' then
              RaiseErr('Could not find "record" after "packed"');
          end;
        SkipWhite;
        while UpperCase(PeekSymbol) <> 'END' do
          begin
            if UpperCase(PeekSymbol) = 'CASE' then
              ParseCase
            else
              Result.Fields.Add( ParseVar );
          end;
        ReadSymbol; // pass the 'end'
        SkipSemiColon;
      except
        Result.Free;
        raise;
      end;
    end;
end;

// XXX = set of YYY
function  TUnit.ParseSet( const TypeName : String ) : TSet;
begin
  with FParser do
    begin
      Result := TSet.Create;
      try
        Result.Name := TypeName;
        ReadSymbol;
        SkipWhite;
        if UpperCase( ReadSymbol ) <> 'OF' then
          RaiseErr('could not find "of" while parsing a set');
        SkipWhite;
        if PeekChar = '(' then
          Result.TypeOfSet := ParseEnum('')
        else
          Result.TypeOfSet := ParseNewType('');
      except
        Result.Free;
        raise;
      end;
    end;
end;

// XXX = array [a..b] of YYY;
function TUnit.ParseArray( const TypeName : String; isPacked : Boolean ) : TArray;

  function ParseDim : TArrayDim;
  begin
    Result := TArrayDim.Create;
    try
      with FParser do
        begin
          Result.Low := ReadExpr;
          SkipWhite;
          if PeekChar <> '.' then
            begin
              Result.TypeInterval := Result.Low;
              Result.Low := '';
            end
          else
            begin
              if ReadChar <> '.' then
                RaiseErr('Could not find ".." when parsing an array');
              if ReadChar <> '.' then
                RaiseErr('Could not find ".." when parsing an array');
              SkipWhite;
              Result.Hi := ReadExpr;
              SkipWhite;
            end;
        end;
    except
      Result.Free;
      raise;
    end;
  end;

begin
  with FParser do
    begin
      Result := TArray.Create;
      try
        Result.Name := TypeName;
        Result.IsPacked := isPacked;
        ReadSymbol;
        SkipWhite;
        if PeekChar = '[' then
          begin
            if ReadChar <> '[' then
              RaiseErr('Could not find "[" when parsing an array');
            SkipWhite;
            while PeekChar <> ']' do
              begin
                Result.Items.Add( ParseDim );
                if PeekChar = ',' then
                  begin
                    ReadChar; // pass ','
                    SkipWhite;
                  end;
              end;
            ReadChar; // pass ']'
            SkipWhite;
          end;
        if UpperCase(ReadSymbol) <> 'OF' then
          RaiseErr('Could not find "of" when parsing an array');
        SkipWhite;
        Result.TypeOfArray := ReadSymbol;
        SkipSemiColon;
      except
        Result.Free;
        raise;
      end;
    end;
end;

// XXX = function (arg1, arg2...argn) : YYY [of object];
function TUnit.ParseTypeFunc( const TypeName : String ) : TTypeFunc;
begin
  with FParser do
    begin
      Result := TTypeFunc.Create;
      try
        Result.Name := TypeName;
        ReadSymbol;
        SkipWhite;
        Result.Args := ParseArgs;
        if ReadChar <> ':' then
          RaiseErr('Could not find ":" when parsing a function');
        SkipWhite;
        Result.ReturnType := ReadSymbol;
        SkipWhite;
        if UpperCase(PeekSymbol) = 'OF' then
          begin
            ReadSymbol; // pass 'of'
            SkipWhite;
            if UpperCase(ReadSymbol) <> 'OBJECT' then
              RaiseErr('could not find "object" after "of"');
            Result.IsOfObject := True;
            SkipWhite;
          end
        else if PeekChar <> ';' then
          begin
            Result.CallType := ReadSymbol;
            SkipWhite;
            if PeekChar <> ';' then
              ReadSymbol;
          end;
        SkipSemiColon;
        Result.Modifiers := ParseModifiers;
      except
        Result.Free;
        raise;
      end;
    end;
end;

// XXX = procedure (arg1, arg2...argn) [of object];
function TUnit.ParseTypeProc( const TypeName : String ) : TTypeProc;
begin
  with FParser do
    begin
      Result := TTypeProc.Create;
      try
        Result.Name := TypeName;
        ReadSymbol;
        SkipWhite;
        Result.Args := ParseArgs;
        if UpperCase(PeekSymbol) = 'OF' then
          begin
            ReadSymbol; // pass 'of'
            SkipWhite;
            if UpperCase(ReadSymbol) <> 'OBJECT' then
              RaiseErr('could not find "object" after "of"');
            Result.IsOfObject := True;
            SkipWhite;
          end
        else if PeekChar <> ';' then
          begin
            Result.CallType := ReadSymbol;
            SkipWhite;
          end;
        SkipSemiColon;
        Result.Modifiers := ParseModifiers;
      except
        Result.Free;
        raise;
      end;
    end;
end;

function  TUnit.ParseProperty : TProperty;
  function ReadPart : String;
  begin
    with FParser do
      begin
        Result := ReadSymbol;
        while PeekChar = '.' do
          begin
            Result := Result + ReadChar; // get the .
            Result := Result + ReadSymbol;
          end;
      end;
  end;
  function ReadDefault : String;
  begin
    Result := '';
    with FParser do
      begin
        if PeekChar = '[' then
          begin
            ReadChar; // pass the [
            SkipWhite;
            while PeekChar <> ']' do
              begin
                Result := Result + ReadExpr;
                SkipWhite;
                if PeekChar = ',' then
                  begin
                    ReadChar; // pass the ,
                    SkipWhite;
                    Result := Result + ', ';
                  end;
              end;
            if ReadChar <> ']' then
              RaiseErr('Could not find a matching "]"');
            Result := '[' + Result + ']';
          end
        else if PeekChar = '#' then
          begin
            Result := ReadChar;
            Result := Result + ReadSymbol;
          end
        else
          Result := ReadConstant;
      end;
  end;

  function ReadIndex : String;
  var
    pDepth : Integer;
    sValue : String;
  begin
    Result := '';
    with FParser do
      begin
        pDepth := ParenthesisDepth;
        repeat
          sValue := ReadUntil( [' ',';'] );
          Result := Result + sValue;
          ReadChar;
        until ParenthesisDepth = pDepth;
        SkipWhite;
      end;
  end;

  function ReadImplements : String;
  var
    sValue : String;
  begin
    Result := '';
    with FParser do
      begin
        repeat
          sValue := ReadUntil( [' ', ',', ';'] );
          Result := Result + sValue;
          SkipWhite;
          if PeekChar = ',' then
            begin
              Result := Result + ',';
              ReadChar; // pass the ,
              SkipWhite;
            end
          else
            Break;
        until False;
        SkipWhite;
      end;
  end;

var
  s : String;
begin
  with FParser do
    begin
      Result := TProperty.Create;
      try
        if UpperCase(ReadSymbol) <> 'PROPERTY' then
          RaiseErr('Could not find "property"');
        SkipWhite;
        Result.Name := ReadSymbol;
        SkipWhite;
        if PeekChar = ';' then // case of: property XXX;
          begin
            SkipSemiColon;
            Exit;
          end;
        if PeekChar = '[' then
          Result.Args := ParseArgs;
        if PeekChar = ':' then
          begin
            ReadChar; // pass ':'
            SkipWhite;
            Result.ReturnType := ReadSymbol;
            SkipWhite;
          end;
        while PeekChar <> ';' do
          begin
            s := UpperCase(ReadSymbol);
            SkipWhite;
            if s = 'READ' then
              Result.PropRead := ReadPart
            else if s = 'WRITE' then
              Result.PropWrite := ReadPart
            else if s = 'DEFAULT' then
              Result.PropDefault := ReadDefault
            else if s = 'DISPID' then
              Result.PropDispId := ReadDefault
            else if s = 'READONLY' then
              Result.IsReadOnly := True
            else if s = 'INDEX' then
              Result.PropIndex := ReadIndex
            else if s = 'IMPLEMENTS' then
              Result.PropImplements := ReadImplements
            else if s = 'STORE' then
              Result.PropStore := ReadSymbol;
            SkipWhite;
          end;
        SkipSemiColon;
        if UpperCase(PeekSymbol) = 'DEFAULT' then
          begin
            ReadSymbol; // pass the 'default' keyword
            SkipSemiColon;
          end;
      except
        Result.Free;
        raise;
      end;
    end;
end;

function  TUnit.ParseConstructor : TConstructor;
begin
  with FParser do
    begin
      Result := TConstructor.Create;
      try
        if UpperCase(ReadSymbol) <> 'CONSTRUCTOR' then
          RaiseErr('Could not find "constructor"');
        SkipWhite;
        Result.Name := ReadSymbol;
        Result.Args := ParseArgs;
        SkipSemiColon;
        Result.Modifiers := ParseModifiers;
      except
        Result.Free;
        raise;
      end;
    end;
end;

function  TUnit.ParseDestructor : TDestructor;
begin
  with FParser do
    begin
      Result := TDestructor.Create;
      try
        if UpperCase(ReadSymbol) <> 'DESTRUCTOR' then
          RaiseErr('Could not find "destructor"');
        SkipWhite;
        Result.Name := ReadSymbol;
        Result.Args := ParseArgs;
        SkipSemiColon;
        Result.Modifiers := ParseModifiers;
      except
        Result.Free;
        raise;
      end;
    end;
end;

function  TUnit.ParseArgs : TArgs;

  function ParseArg : TArg;
  var
    s, sValue : String;
    pDepth : Integer;
  begin
    with FParser do
      begin
        Result := TArg.Create;
        try
          SkipWhite;
          s := ReadSymbol;
          SkipWhite;
          if UpperCase( s ) = 'VAR' then
            Result.IsVar := True
          else if UpperCase( s ) = 'CONST' then
            Result.IsConst := True
          else if UpperCase( s ) = 'OUT' then
            Result.IsOut := True
          else
            begin
              Result.Items.Add( s );
              if PeekChar = ',' then
                begin
                  ReadChar; // pass the ','
                  SkipWhite;
                end;
            end;
          while PeekChar <> ':' do
            begin
              Result.Items.Add( ReadSymbol );
              SkipWhite;
              if PeekChar = ',' then
                begin
                  ReadChar; // pass ','
                  SkipWhite;
                end
              else if PeekChar <> ':' then
                Exit;
            end;
          ReadChar; // pass ':'
          SkipWhite;
          s := UpperCase(PeekSymbol);
          if s = 'ARRAY' then
            begin
              ReadSymbol; // pass 'array'
              SkipWhite;
              s := UpperCase(ReadSymbol);
              if s <> 'OF' then
                RaiseErr('could not find "of" in an arg');
              SkipWhite;
              Result.IsArrayOf := True;
            end;
          Result.Name := ReadSymbol;
          SkipWhite;
          s := UpperCase(PeekChar);
          if s = '=' then
            begin
              ReadChar; // pass '='
              SkipWhite;
              sValue := '';
              pDepth := ParenthesisDepth;
              repeat
                sValue := sValue + ReadUntil( [';', ')'] );
                if ParenthesisDepth > pDepth then
                  begin
                    sValue := sValue + ')';
                    ReadChar; // pass the )
                  end;
              until ParenthesisDepth = pDepth;
              SkipWhite;
              Result.DefaultValue := sValue;
            end;
        except
          Result.Free;
          raise;
        end;
      end;
  end;

begin
  with FParser do
    begin
      Result := TArgs.Create;
      try
        SkipWhite;
        if not (PeekChar in ['(', '[']) then
          Exit;
        ReadChar; // pass '(' or '['
        while not (PeekChar in [')', ']']) do
          begin
            Result.Items.Add( ParseArg );
            if PeekChar = ';' then
              begin
                ReadChar; // pass ';'
                SkipWhite;
              end;
          end;
        ReadChar; // pass ')' or ']'
        SkipWhite;
      except
        Result.Free;
        raise;
      end;
    end;
end;

function  TUnit.ParseModifiers : TModifiers;

  function ParseModifier( modif : TModifiers ) : Boolean;
  var
    s, ext, tmp : String;
  begin
    with FParser do
      begin
        Result := False;
        SkipWhite;
        s := UpperCase( PeekSymbol );
        if (s = 'VIRTUAL') or
           (s = 'ABSTRACT') or
           (s = 'DYNAMIC' ) or
           (s = 'OVERRIDE' ) or
           (s = 'OVERLOAD' ) or
           (s = 'REINTRODUCE' ) or
           (s = 'FORWARD') or
           (s = 'ASSEMBLER') or
           (s = 'PASCAL') or
           (s = 'REGISTER') or
           (s = 'STDCALL') or
           (s = 'SAFECALL') or
           (s = 'CDECL') then
          begin
            modif.Items.Add( ReadSymbol );
            SkipSemiColon;
            Result := True;
          end
        else if (s = 'MESSAGE') and (PeekCharAfterSymbol <> ':') then
          begin
            ReadSymbol; // pass 'message'
            SkipWhite;
            modif.Items.Add( Format('message %s', [ReadConstant]) );
            SkipSemiColon;
            Result := True;
          end
        else if s = 'DISPID' then
          begin
            ReadSymbol; // pass 'dispid'
            SkipWhite;
            modif.Items.Add( Format('dispid %s', [ReadExpr]) );
            SkipSemiColon;
            Result := True;
          end
        else if s = 'EXTERNAL' then
          begin
            ReadSymbol; // pass 'external'
            SkipWhite;
            ext := ReadConstant;
            SkipWhite;
            tmp := ext;
            if PeekChar <> ';' then
              begin
                s := UpperCase( PeekSymbol );
                if s = 'NAME' then
                  begin
                    ReadSymbol; // pass 'name'
                    SkipWhite;
                    tmp := tmp + ' name ' + ReadConstant;
                  end                              
                else if s = 'INDEX' then
                  begin
                    ReadSymbol; // pass 'index'
                    SkipWhite;
                    tmp := tmp + ' index ' + ReadConstant;
                  end;
                SkipWhite;
              end;
            modif.Items.Add( tmp );
            SkipSemiColon;
            Result := True;
          end;
      end;
  end;

begin
  with FParser do
    begin
      Result := TModifiers.Create;
      try
        repeat
        until not ParseModifier(Result);
      except
        Result.Free;
        raise;
      end;
    end;
end;

function  TUnit.IsSection : Boolean;
var
  s : String;
begin
  with FParser do
    begin
      SkipWhite;
      s := UpperCase( PeekSymbol );
      if (s = 'IMPLEMENTATION') or
         (s = 'TYPE') or
         (s = 'CONST') or
         (s = 'RESOURCESTRING') or
         (s = 'THREADVAR') or
         (s = 'VAR') then
        Result := True
      else
        Result := False;
    end;
end;

function TUnit.IsCallType : Boolean;
var
  s : String;
begin
  with FParser do
    begin
      SkipWhite;
      s := UpperCase( PeekSymbol );
      if (s = 'PASCAL') or
         (s = 'REGISTER') or
         (s = 'STDCALL') or
         (s = 'SAFECALL') or
         (s = 'CDECL') then
        Result := True
      else
        Result := False;
    end;
end;

procedure TUnit.AddToTree( tree : TTreeView; node : TTreeNode );
var
  n, n2 : TTreeNode;
  i : Integer;
begin
  // add unit
  n := tree.Items.Add( node, Format( 'unit %s', [Name]) );
  // add uses
  n2 := tree.Items.AddChild( n, 'uses' );
  for i := 0 to FUsesList.Count - 1 do
    tree.Items.AddChild( n2, FUsesList.Strings[i] );
  // add consts
  n2 := tree.Items.AddChild( n, 'const' );
  for i := 0 to ConstCount - 1 do
    Consts[i].AddToTree( tree, n2 );
  // add resourcestrings
  n2 := tree.Items.AddChild( n, 'resourcestring' );
  for i := 0 to ResourceStringCount - 1 do
    ResourceStrings[i].AddToTree( tree, n2 );
  // add types
  n2 := tree.Items.AddChild( n, 'type' );
  for i := 0 to TypeCount - 1 do
    Types[i].AddToTree( tree, n2 );
  // add vars
  n2 := tree.Items.AddChild( n, 'var' );
  for i := 0 to VarCount - 1 do
    Vars[i].AddToTree( tree, n2 );
  // add ThreadVars
  if ThreadVarCount > 0 then
    begin
      n2 := tree.Items.AddChild( n, 'threadvar' );
      for i := 0 to ThreadVarCount - 1 do
        ThreadVars[i].AddToTree( tree, n2 );
    end;
end;

function TUnit.AsString : String;
begin
  Result := '';
end;


end.
