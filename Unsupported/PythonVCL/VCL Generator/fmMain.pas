unit fmMain;

(**************************************************************************)
(*                                                                        *)
(* VCL Generator                       Copyright (c) 1997                 *)
(*                                                                        *)
(* Version: 1                          Morgan Martinet                    *)
(* SubVersion: 0                       23 rue du 14 juillet               *)
(* Date: 27/01/1998                    94270 le Kremlin-Bicetre           *)
(*                                     Phone (Work): 01 47 25 70 77       *)
(*                                     e-mail: mmm@imaginet.fr            *)
(*                                                                        *)
(**************************************************************************)
(* Purpose: Parse the Delphi units of the VCL and try to generate Python  *)
(*          maping objects in order to use the Delphi VCL into Python.    *)
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

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, Menus, unUnitParser, StdCtrls, ExtCtrls, unHash;

type
  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    About1: TMenuItem;
    About2: TMenuItem;
    Quit1: TMenuItem;
    TreeView1: TTreeView;
    StatusBar1: TStatusBar;
    N1: TMenuItem;
    OpenDialog1: TOpenDialog;
    New1: TMenuItem;
    Open2: TMenuItem;
    Save1: TMenuItem;
    Saveas1: TMenuItem;
    Edit1: TMenuItem;
    Execute1: TMenuItem;
    Generate1: TMenuItem;
    AddDelphiunit1: TMenuItem;
    RemoveDelphiunit1: TMenuItem;
    ClearallDelphiunits1: TMenuItem;
    Splitter1: TSplitter;
    Panel1: TPanel;
    ListBox1: TListBox;
    OpenDialog2: TOpenDialog;
    SaveDialog1: TSaveDialog;
    N2: TMenuItem;
    Options1: TMenuItem;
    N3: TMenuItem;
    Settings1: TMenuItem;
    procedure Quit1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure About2Click(Sender: TObject);
    procedure AddDelphiunit1Click(Sender: TObject);
    procedure ClearallDelphiunits1Click(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure Open2Click(Sender: TObject);
    procedure Saveas1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure RemoveDelphiunit1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Options1Click(Sender: TObject);
    procedure Generate1Click(Sender: TObject);
    procedure Settings1Click(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
  private
    { Déclarations privées }
    FUnits : TList;
    FSymbols : THashTable;
    FList1 : TStringList;
    FList2 : TStringList;
    FList3 : TStringList;
    FList4 : TStringList;
    FMethodCount : Integer;
    FBadMethodCount : Integer;
    FBadResultCount : Integer;

    function GetUnitCount : Integer;
    function GetUnits( idx : Integer ) : TUnit;

  public
    { Déclarations publiques }
    FChanged : Boolean;

    procedure HandleFile( const FileName : String );
    procedure SaveProject;
    procedure LoadProject;
    procedure GenerateUnit( AUnit : TUnit );
    procedure BuildSymbolsOf( AUnit : TUnit );
    function  IsClass( const Name : String ) : Boolean;
    function  IsStrictClass( const Name : String ) : Boolean;
    function  IsIntType( const val : String ) : Boolean;
    function  IsRealType( const val : String ) : Boolean;
    function  IsStringType( const val : String ) : Boolean;
    function  IsCharType( const val : String ) : Boolean;
    function  IsBooleanType( const val : String ) : Boolean;
    function  IsEnumType( const val : String ) : Boolean;
    function  IsSetType( const val : String ) : Boolean;
    function  IsBasicType( const val : String ) : Boolean;
    function  FindRootType( const val : String ) : String;
    procedure FillListsWithArgs( args : TArgs );
    procedure FindConstructors( cl : TMyClass; list : TStrings );
    function  FindConstructor( cl : TMyClass ) : TConstructor;
    function  FindArgsOfConstructor( c : TConstructor ) : TArgs;
    function  FindOriginalProperty( cl : TMyClass; prop : TProperty ) : TProperty;
    function  FindUnit( const typeName : String ) : TUnit;
    function  GetParentClass( cl : TMyClass ) : String;
    function  IsInterval( p : TPascalItem ) : Boolean;
    function  IsSymbolExcluded( const symbol : String ) : Boolean;

    property UnitCount : Integer read GetUnitCount;
    property Units[idx : Integer] : TUnit read GetUnits;
    property Symbols : THashTable read FSymbols write FSymbols;
  end;

  procedure ClearLog;
  procedure Log( const text : String );

var
  MainForm: TMainForm;

implementation

uses fmAbout, fmOptions, unMisc, fmSettings, PyRecords;

{$R *.DFM}

procedure TMainForm.Quit1Click(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.HandleFile( const FileName : String );
var
  L : TStringList;
  F : TextFile;
  s : String;
  U : TUnit;
begin
  if not FileExists( FileName ) then
    raise Exception.CreateFmt( 'File %s does not exist', [FileName] );
  StatusBar1.SimpleText := 'Parsing file '+FileName;
  StatusBar1.Update;
  L := TStringList.Create;
  try
    Screen.Cursor := crHourGlass;
    AssignFile( F, FileName );
    Reset(F);
    while not eof(F) do
      begin
        ReadLn( F, s );
        L.Add( s );
        s := UpperCase(Trim(s));
        if (s = 'IMPLEMENTATION') or (s = 'END.') then
          Break;
      end;
    CloseFile(F);
    U := TUnit.Create;
    FUnits.Add( U );
    U.Parse( L.Text );
    U.AddToTree( TreeView1, nil );
  finally
    L.Free;
    Screen.Cursor := crDefault;
    StatusBar1.SimpleText := '';
  end;
  BuildSymbolsOf( U );
end;

function TMainForm.GetUnitCount : Integer;
begin
  Result := FUnits.Count;
end;

function TMainForm.GetUnits( idx : Integer ) : TUnit;
begin
  Result := TUnit(FUnits.Items[idx]);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  i : Integer;
begin
  for i := 0 to UnitCount - 1 do
    Units[i].Free;
  FUnits.Free;
  FSymbols.Free;
  FList1.Free;
  FList2.Free;
  FList3.Free;
  FList4.Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FUnits := TList.Create;
  FSymbols := THashTable.Create( False );
  FList1 := TStringList.Create;
  FList2 := TStringList.Create;
  FList3 := TStringList.Create;
  FList4 := TStringList.Create;
end;

procedure TMainForm.About2Click(Sender: TObject);
begin
  AboutBox.ShowModal;
end;

procedure TMainForm.AddDelphiunit1Click(Sender: TObject);
var
  i : Integer;
begin
  with OpenDialog1 do
    begin
      if Execute then
        begin
          TreeView1.Items.BeginUpdate;
          try
            for i := 0 to Files.Count - 1 do
              begin
                HandleFile( Files.Strings[i] );
                ListBox1.Items.Add( Files.Strings[i] );
              end;
          finally
            TreeView1.Items.EndUpdate;
          end;
          FChanged := True;
        end
    end;
end;

procedure TMainForm.ClearallDelphiunits1Click(Sender: TObject);
var
  i : Integer;
begin
  for i := 0 to UnitCount - 1 do
    Units[i].Free;
  FUnits.Clear;
  FSymbols.Clear;
  TreeView1.Items.Clear;
  ListBox1.Items.Clear;
  FChanged := True;
end;

procedure TMainForm.New1Click(Sender: TObject);
begin
  ClearallDelphiunits1Click( ClearallDelphiunits1 );
  ListBox1.Clear;
  FChanged := False;
  SaveDialog1.FileName := '';
end;

procedure TMainForm.Open2Click(Sender: TObject);
begin
  with OpenDialog2 do
    begin
      if not Execute then
        Exit;
      LoadProject;
    end;
end;

procedure TMainForm.Saveas1Click(Sender: TObject);
begin
  with SaveDialog1 do
    begin
      if not Execute then
        Exit;
      SaveProject;
    end;
end;

procedure TMainForm.Save1Click(Sender: TObject);
begin
  if SaveDialog1.FileName = '' then
    Saveas1Click( Saveas1 )
  else
    SaveProject;
end;

procedure TMainForm.SaveProject;

  procedure Save( const FileName : String );
  var
    S : TFileStream;
  begin
    S := TFileStream.Create( FileName, fmCreate );
    try
      Settings.SaveToStream( S );
      ListBox1.Items.SaveToStream( S );
    finally
      S.Free;
    end;
  end;

begin
  Save( SaveDialog1.FileName );
  FChanged := False;
end;

procedure TMainForm.LoadProject;

  procedure Load( const FileName : String );
  var
    S : TFileStream;
  begin
    S := TFileStream.Create( FileName, fmOpenRead );
    try
      Settings.LoadFromStream( S );
      ListBox1.Items.LoadFromStream( S );
    finally
      S.Free;
    end;
  end;

var
  i : Integer;
begin
  New1Click( New1 );
  Load( OpenDialog2.FileName );
  SaveDialog1.FileName := OpenDialog2.FileName;
  for i := 0 to ListBox1.Items.Count - 1 do
    HandleFile( ListBox1.Items.Strings[i] );
end;

procedure TMainForm.Edit1Click(Sender: TObject);
begin
  RemoveDelphiunit1.Enabled := (ListBox1.ItemIndex >= 0) and
                               (ListBox1.Items.Count > 0);
end;

procedure TMainForm.RemoveDelphiunit1Click(Sender: TObject);
var
  i : Integer;
begin
  with ListBox1 do
    begin
      Units[ItemIndex].Free;
      FUnits.Delete( ItemIndex );
      Items.Delete( ItemIndex );
      FChanged := True;
      TreeView1.Items.Clear;
      TreeView1.Items.BeginUpdate;
      try
        for i := 0 to FUnits.Count - 1 do
          TPascalItem(FUnits.Items[i]).AddToTree( TreeView1, nil );
      finally
        TreeView1.Items.EndUpdate;
      end;
    end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
const
  msg = 'Your project has changed. Do you want to save it before ?';
var
  dlg : Integer;
begin
  if FChanged then
    begin
      dlg := MessageDlg( msg, mtConfirmation, [mbYes, mbNo, mbCancel], 0 );
      CanClose := dlg <> mrCancel;
      if dlg = mrYes then
        Save1Click( Save1 );
    end;
end;

procedure TMainForm.Options1Click(Sender: TObject);
begin
  Options.ShowModal;
end;

procedure TMainForm.Generate1Click(Sender: TObject);

  procedure GenerateComponent;
  var
    f : TextFile;
    s : String;
    i : Integer;
  begin
    s := Settings.edUnitName.Text;
    if Pos( '.', s ) > 0 then
      Delete( s, Pos( '.', s ), 4 );
    AssignFile( f, AppendSlash(Settings.edDelphiFiles.Text)+s+'.pas' );
    Rewrite( f );
    try
      // Write banner
      WriteLn( f, '/////' );
      WriteLn( f, '//  This file was generated by VCL Generator' );
      WriteLn( f, '//  Copyright 1998 - Morgan Martinet' );
      WriteLn( f, '//  ', DateTimeToStr(Now) );
      WriteLn( f, '//  it builds a component that will install the Python/Delphi mapping' );
      WriteLn( f, '/////' );
      WriteLn( f, '' );
      // Write Unit name
      WriteLn( f, 'unit ' + s + ';' );
      WriteLn( f, '');
      // Write Interface section
      WriteLn( f, 'interface' );
      WriteLn( f, '');
      WriteLn( f, 'uses Classes, SysUtils, PythonEngine;' );
      WriteLn( f, '');
      WriteLn( f, 'type' );
      WriteLn( f, '  ', Settings.edComponentName.Text, ' = class( TEngineClient )' );
      WriteLn( f, '  protected' );
      WriteLn( f, '    procedure SetEngine( val : TPythonEngine ); override;' );
      WriteLn( f, '' );
      WriteLn( f, '  public' );
      WriteLn( f, '  end;' );
      WriteLn( f, '' );
      WriteLn( f, '  procedure Register;' );
      WriteLn( f, '' );
      // Write Implementation section
      WriteLn( f, 'implementation' );
      WriteLn( f, '');
      // Write uses instruction
      WriteLn( f, 'uses' );
      for i := 0 to UnitCount - 1 do
        WriteLn( f, '   Python_', Units[i].Name, ',' );
      WriteLn( f, '   PyVarArg,' );
      WriteLn( f, '   PyRecords,' );
      WriteLn( f, '   PyDelphiAssoc;' );
      WriteLn( f, '' );
      // Register component
      WriteLn( f, 'procedure Register;' );
      WriteLn( f, 'begin' );
      WriteLn( f, '  RegisterComponents( ''Python'', [', Settings.edComponentName.Text, '] );' );
      WriteLn( f, 'end;' );
      WriteLn( f, '' );
      // Init associated modules
      WriteLn( f, 'procedure ', Settings.edComponentName.Text, '.SetEngine( val : TPythonEngine );' );
      WriteLn( f, 'begin');
      WriteLn( f, '  inherited;' );
      WriteLn( f, '  if csDesigning in ComponentState then' );
      WriteLn( f, '     Exit;' );
      WriteLn( f, '  val.AutoFinalize := False;' );
      for i := 0 to UnitCount - 1 do
        WriteLn( f, '  Python_', Units[i].Name, '.Init( Owner, val );' );
      WriteLn( f, '  PyDelphiAssoc.Init( Owner, val );' );
      WriteLn( f, '  PyVarArg.Init( Owner, val );' );
      WriteLn( f, '  PyRecords.Init( Owner, val );' );
      WriteLn( f, 'end;');
      WriteLn( f, '');
      // Write end of unit
      WriteLn( f, 'end.' );
    finally
      CloseFile( f );
    end;
  end;

var
  i : Integer;
begin
  FMethodCount := 0;
  FBadMethodCount := 0;
  FBadResultCount := 0;
  ClearLog;
  Log( Format( 'Generation of %s', [OpenDialog2.FileName]) );
  Log( Format( 'Generation started at %s, %s', [TimeToStr(Now), DateToStr(Now)]) );
  Screen.Cursor := crHourGlass;
  try
    for i := 0 to UnitCount - 1 do
      GenerateUnit( Units[i] );
    GenerateComponent;
  finally
    Screen.Cursor := crDefault;
    StatusBar1.SimpleText := '';
    Log( '##################################################################' );
    Log( Format( 'Generation finished at %s, %s', [TimeToStr(Now), DateToStr(Now)]) );
    Log( '' );
    Log( Format('%d methods generated.', [FMethodCount]) );
    Log( Format('%d methods not properly implemented (%0.2f%%).',
         [FBadMethodCount, FBadMethodCount * 100.0 / FMethodCount ]) );
    Log( Format('%d methods whose result is not implemented (%0.2f%%).',
         [FBadResultCount, FBadResultCount * 100.0 / FMethodCount ]) );
  end;
end;

procedure TMainForm.GenerateUnit( AUnit : TUnit );

  function IsGlobalObject( const aClass : String ) : Boolean;
  var
    i : Integer;
  begin
    with Settings.meGlobals.Lines do
      begin
        Result := False;
        for i := 0 to Count - 1 do
          if CompareText( Strings[i], aClass ) = 0 then
            begin
              Result := True;
              Break;
            end;
      end;
  end;

  function IsUnitPresent( const name : String ) : Boolean;
  var
    i : Integer;
  begin
    Result := False;
    for i := 0 to UnitCount - 1 do
      if CompareText( name, Units[i].Name ) = 0 then
        begin
          Result := True;
          Break;
        end;
  end;

  procedure GeneratePythonFile;

    procedure GenerateConstructors( var f : TextFile; cl : TMyClass );
    var
      i, j : Integer;
      n, s, tmp : String;
      list : TStringList;
    begin
      list := TStringList.Create;
      try
        FindConstructors( cl, list );
        for i := 0 to list.Count-1 do
          if list.Objects[i] is TConstructor then
            with TConstructor(list.Objects[i]) do
              begin
                s := 'Self';
                args := FindArgsOfConstructor( TConstructor(list.Objects[i]) );
                if Assigned(args) then
                  with args do
                    begin
                      for j := 0 to FList1.Count - 1 do
                        begin
                          tmp := FList1.Strings[j];
                          if FList4.Strings[j] <> '' then
                            tmp := tmp + ' = ' + FList4.Strings[j];
                          s := s + ', ' + tmp;
                        end;
                    end;
                // constructor
                WriteLn( f, '    def ', Name,'( ', s, ' ):' );
                n := Copy(cl.Name, 2, Length(cl.Name)-1);
                s := 'Self';
                if Assigned(args) then
                  with args do
                    begin
                      for j := 0 to FList1.Count - 1 do
                        s := s + ', ' + FList1.Strings[j];
                    end;
                WriteLn( f, '        return _', AUnit.Name,'.', Name, n, '( ', s, ' )' );
                WriteLn( f, '' );
              end;
      finally
        list.Free;
      end;
    end;

  var
    f : TextFile;
    i, j : Integer;
    s, n : String;
  begin
    AssignFile( f, AppendSlash(Settings.edPythonFiles.Text)+AUnit.Name+'.py' );
    Rewrite( f );
    with AUnit do
    try
      // Write banner
      WriteLn( f, '###' );
      WriteLn( f, '#  This file was generated by VCL Generator' );
      WriteLn( f, '#  Copyright 1998 - Morgan Martinet' );
      WriteLn( f, '#  ', DateTimeToStr(Now) );
      WriteLn( f, '#  it declares the symbols of the Delphi unit ', Name, '.pas' );
      WriteLn( f, '###' );
      WriteLn( f, '' );
      // Generate imports
      if CompareText( Name, 'System' ) <> 0 then
        WriteLn( f, 'from System import *' );
      for i := 0 to UsesList.Count - 1 do
        if IsUnitPresent( UsesList.Strings[i] ) then
          WriteLn( f, 'from ', UsesList.Strings[i], ' import *' );
      WriteLn( f, 'import _', Name );
      WriteLn( f, '' );
      // Generate constants for Enums and Sets
      for i := 0 to TypeCount - 1 do
        if Types[i] is TEnum then
          with TEnum(Types[i]) do
            begin
              WriteLn( f, '# ', AsString );
              for j := 0 to Items.Count - 1 do
                WriteLn( f, Items.Strings[j], ' = ', IntToStr(j) );
              WriteLn( f, '' );
            end
        else if Types[i] is TSet then
          with TSet(Types[i]) do
            begin
              if not (TypeOfSet is TEnum) then
                Continue;
              WriteLn( f, '# ', AsString );
              with TEnum(TypeOfSet) do
                begin
                  for j := 0 to Items.Count - 1 do
                    WriteLn( f, Items.Strings[j], ' = ', IntToStr(j) );
                  WriteLn( f, '' );
                end
            end;
      // Generate classes
      for i := 0 to TypeCount - 1 do
        if (Types[i] is TMyClass) and not (Types[i] is TMyInterface) then
          with TMyClass(Types[i]) do
            begin
              // We skip the excluded symbols
              if IsSymbolExcluded( Name ) then
                Continue;
              // We skip the exceptions (classes that begins with E)
              if Name[1] = 'E' then
                Continue;
              if CompareText( AUnit.Name, 'System' ) = 0 then
                begin
                  WriteLn( f, '# Boolean constants' );
                  WriteLn( f, 'True = 1' );
                  WriteLn( f, 'False = 0' );
                  WriteLn( f, '' );
                end;
              if ParentClasses.Count > 0 then
                s := '( ' + ParentClasses.Strings[0] + ' )'
              else if CompareText( Name, 'TObject' ) <> 0 then
                s := '(TObject)'
              else
                s := '';
              WriteLn( f, '####################################################' );
              WriteLn( f, 'class ', Name, s, ':' );
              // constructors
              GenerateConstructors( f, TMyClass(Types[i]) );
              {s := 'Self';
              constr := FindConstructor( TMyClass(Types[i]) );
              if Assigned(constr) then
                args := FindArgsOfConstructor( constr )
              else
                args := nil;
              if Assigned(constr) and Assigned(args) then
                with args do
                  begin
                    for j := 0 to FList1.Count - 1 do
                      s := s + ', ' + FList1.Strings[j];
                  end;
              WriteLn( f, '    def __init__( ', s, ' ):' );
              WriteLn( f, '        _', AUnit.Name,'.Create', n, '( ', s, ' )' );
              WriteLn( f, '' );}
              n := Copy(Name, 2, Length(Name)-1);
              // get attribute
              WriteLn( f, '    def __getattr__( Self, Key ):' );
              WriteLn( f, '        return _', AUnit.Name,'.', n, '_GetAttr( Self, Key )' );
              WriteLn( f, '' );
              // set attribute
              WriteLn( f, '    def __setattr__( Self, Key, Value ):' );
              WriteLn( f, '        return _', AUnit.Name,'.', n, '_SetAttr( Self, Key, Value )' );
              WriteLn( f, '' );
              // Special case: root of all objects
              if CompareText( Name, 'TObject' ) = 0 then
                begin
                  WriteLn( f, '    def __str__( Self ):' );
                  WriteLn( f, '        return "<Instance of "+Self.__class__.__name__+" at "+hex(id(Self))+">"' );
                  WriteLn( f, '' );
                  WriteLn( f, '    def __repr__( Self ):' );
                  WriteLn( f, '        return str(Self)' );
                  WriteLn( f, '' );
                  WriteLn( f, '    def __hex__( Self ):' );
                  WriteLn( f, '        return hex(id(Self))' );
                  WriteLn( f, '' );
                  WriteLn( f, '    def Free( Self ):' );
                  WriteLn( f, '        if Self.__dict__.has_key("__assoc__"):' );
                  WriteLn( f, '            del Self.__assoc__' );
                  WriteLn( f, '' );
                end;
              if IsGlobalObject( Name ) then
                begin
                  WriteLn( f, Copy( Name, 2, Length(Name)-1), ' = ', Name, '().Create( None )' );
                  WriteLn( f, '' );
                end;
            end;
    finally
      CloseFile( f );
    end;
  end;

  procedure GeneratePythonInterface;

    function GetConstructorName( const clName, constrName : String ) : String;
    begin
      Result := constrName;
    end;

    procedure GenerateMethArgsDecl( var f : TextFile; args : TArgs );
    var
      i : Integer;
      s : String;
      T : TPascalItem;
    begin
      if not Assigned(args) then
        Exit;
      with args do
        for i := 0 to FList1.Count - 1 do
          begin
            T := TPascalItem(Symbols.Find(FList2.Strings[i]));
            s := FindRootType( FList2.Strings[i] );
            if IsClass( FList2.Strings[i] ) then
              begin
                WriteLn( f, '  _arg', FList1.Strings[i], ' : PPyObject;' );
                WriteLn( f, '  _obj', FList1.Strings[i], ' : ', FList2.Strings[i], ';' );
              end
            else if IsIntType(s) or IsBooleanType(s) or IsEnumType(s) or IsInterval(T) then
              WriteLn( f, '  _arg', FList1.Strings[i], ' : Integer;' )
            else if IsSetType(s) then
              begin
                WriteLn( f, '  _arg', FList1.Strings[i], ' : PPyObject;' );
                WriteLn( f, '  _tmp', FList1.Strings[i], ' : ', FList2.Strings[i], ';' );
              end
            else if IsRealType(s) then
              WriteLn( f, '  _arg', FList1.Strings[i], ' : Double;' )
            else if IsStringType(s) then
              WriteLn( f, '  _arg', FList1.Strings[i], ' : PChar;' )
            else if IsCharType(s) then
              WriteLn( f, '  _arg', FList1.Strings[i], ' : PPyObject;' )
            else
              WriteLn( f, '  _arg', FList1.Strings[i], ' : PPyObject;' )
          end;
    end;

    procedure GenerateMethArgsExtr( var f : TextFile; args : TArgs; extractSelf : Boolean; const errorMsg : String );
    var
      i : Integer;
      s, sep : String;
      T : TPascalItem;
    begin
      if (FList2.Count = 0) and not extractSelf  then
        Exit;
      with args do begin
        Write( f, '    if PyArg_ParseTuple( Args, ''' );
        if extractSelf then
          Write( f, 'O' ); // for Self
        for i := 0 to FList2.Count - 1 do
          begin
            T := TPascalItem(Symbols.Find(FList2.Strings[i]));
            if IsClass( FList2.Strings[i] ) then
               Write( f, 'O' )
            else
              begin
                s := FindRootType( FList2.Strings[i] );
                if IsIntType(s)  or IsBooleanType(s) or IsEnumType(s) or IsInterval(T) then
                  Write( f, 'i' )
                else if IsSetType(s) then
                  Write( f, 'O' )
                else if IsRealType(s) then
                  Write( f, 'f' )
                else if IsStringType(s) then
                  Write( f, 's' )
                else if IsCharType(s) then
                  Write( f, 'O' )
                else
                  Write( f, 'O' );
              end;
          end;
        Write( f, ':', errorMsg,''', [ ' );
        if extractSelf then begin
          Write( f, '@_argSelf' );
          sep := ',';
        end
        else
          sep := '';
        for i := 0 to FList1.Count - 1 do
          begin
            WriteLn( f, sep );
            Write( f, '                  @_arg', FList1.Strings[i] );
            sep := ',';
          end;
        WriteLn( f, '       ] ) = 0 then' );
        WriteLn( f, '      begin' );
        WriteLn( f, '        Result := nil;' );
        WriteLn( f, '        Exit;' );
        WriteLn( f, '      end;' );
      end;
    end;

    procedure GenerateMethArgsExpl( var f : TextFile; args : TArgs; extractSelf : Boolean );
    var
      i : Integer;
    begin
      with args do begin
        for i := 0 to FList1.Count - 1 do
          if IsClass( FList2.Strings[i] ) then
            WriteLn( f, '    _obj', FList1.Strings[i], ' := GetObjectOf( _arg',
                     FList1.Strings[i], ' ) as ', FList2.Strings[i],';' )
          else if IsSetType( FList2.Strings[i] ) then
            begin
              WriteLn( f, '    if PyList_Check( _arg', FList1.Strings[i], ' ) then' );
              WriteLn( f, '      ListToSet( _arg', FList1.Strings[i],', @_tmp', FList1.Strings[i], ', sizeof(_tmp', FList1.Strings[i],') )' );
              WriteLn( f, '    else' );
              if extractSelf then
                WriteLn( f, '      ErrorWrongSetPropertyType( _argSelf, ''', FList1.Strings[i], ''', ''', FList2.Strings[i], ''' );' )
              else
                WriteLn( f, '      ErrorWrongSetPropertyType( Self, ''', FList1.Strings[i], ''', ''', FList2.Strings[i], ''' );' );
            end;
        WriteLn( f, '    if PyErr_Occurred <> nil then' );
        WriteLn( f, '      begin' );
        WriteLn( f, '        Result := nil;' );
        WriteLn( f, '        Exit;' );
        WriteLn( f, '      end;' );
      end;
    end;

    function CheckMethArgs( args : TArgs ) : Boolean;
    var
      i : Integer;
      s : String;
      T : TPascalItem;
    begin
      Result := True;
      with args do begin
        if FList1.Count > 0 then
          begin
            for i := 0 to FList1.Count - 1 do
              begin
                T := TPascalItem(Symbols.Find(FList2.Strings[i]));
                s := FindRootType( FList2.Strings[i] );
                Result := IsClass( FList2.Strings[i] ) or
                          (IsIntType(s)  or IsBooleanType(s) or IsEnumType(s) or
                           IsRealType(s) or IsInterval(T) or IsStringType(s)) or
                          IsSetType(s) or
                          IsImplementedRecord(s) or
                          IsCharType(s);
                if not Result then
                  Break;
              end;
          end;
      end;
    end;

    procedure GenerateMethCall( var f : TextFile; args : TArgs; const methName, ResultVar : String; isProperty : Boolean );

      function CanCast( const aType : String ) : Boolean;
      begin
        Result := not( IsRealType(aType) );
      end;

    var
      i : Integer;
      s, sep : String;
      T : TPascalItem;
    begin
      with args do begin
        Write( f, '    ' );
        if ResultVar <> '' then
          Write(   f, ResultVar, ' := ' );
        Write( f, methName );
        if FList1.Count > 0 then
          begin
            if isProperty then
              Write( f, '[ ' )
            else
              Write( f, '( ' );
            sep := ', ';
            for i := 0 to FList1.Count - 1 do
              begin
                if i > 0 then
                  Write( f, sep );
                T := TPascalItem(Symbols.Find(FList2.Strings[i]));
                s := FindRootType( FList2.Strings[i] );
                if IsClass( FList2.Strings[i] ) then
                  Write( f, '_obj', FList1.Strings[i] )
                else if IsIntType(s)  or IsBooleanType(s) or IsEnumType(s) or
                        IsRealType(s) or IsInterval(T) or
                        IsStringType(s) then
                  if CanCast(s) then
                    Write( f, FList2.Strings[i], '(_arg', FList1.Strings[i], ')' )
                  else
                    Write( f, '_arg', FList1.Strings[i] )
                else if IsSetType(s) then
                  Write( f, '_tmp', FList1.Strings[i] )
                else if IsCharType(s) then
                  Write( f, 'PyObjectAsChar(_arg', FList1.Strings[i], ')' )
                else if IsImplementedRecord(s) then
                  Write( f, PyObjectAsRecordWith( s, '_arg'+FList1.Strings[i] ) )
                else
                  Write( f, 'nil' );
              end;
            if isProperty then
              Write( f, ' ]' )
            else
              Write( f, ' )' );
          end;
        WriteLn( f, ';' );
      end;
    end;

    procedure GenerateResultCall( var f : TextFile; args : TArgs; const methName, resultVar, ReturnType, UnitName : String );
    var
      s : String;
      T : TPascalItem;
    begin
      s := FindRootType( ReturnType );
      T := TPascalItem(Symbols.Find(ReturnType));
      Write( f, '    Result := ' );
      if CompareText( s, 'PCHAR' ) = 0 then
        Write( f, 'PyString_FromString( ', resultVar, ' )' )
      else if IsStrictClass( s ) then
        Write( f,  'GetPythonObject( ', resultVar, ', ''', UnitName, ''', ''', ReturnType, ''' )' )
      else if IsSetType(s) then
        Write( f, 'SetToList( @', resultVar, ', sizeof(', resultVar,') )' )
      else if IsEnumType(s) or IsInterval(T) then
        Write( f, 'VariantAsPyObject( Integer( ', resultVar, ' ) )' )
      else if IsImplementedRecord(s) then
        Write( f, MakeRecord( resultVar, s ) )
      else if IsCharType(s)then
        Write( f, 'VariantAsPyObject( ', resultVar, ' )' )
      else if IsBasicType(s)then
        Write( f, 'VariantAsPyObject( ', resultVar, ' )' )
      else
        begin
          Write( f, 'ReturnNone' );
          Log( 'Could not generate result of method: ' + methName + Args.AsString + ' : ' + ReturnType);
          Inc(FBadResultCount);
        end;
      WriteLn( f, ';' );
    end;


    procedure GenerateClassConstructor( var f : TextFile; cl : TMyClass );
    var
      k : Integer;
      clName : String;
      constr : TConstructor;
      list : TStringList;
    begin
      list := TStringList.Create;
      try
        FindConstructors( cl, list );
        for k := 0 to list.Count-1 do
          if list.Objects[k] is TConstructor then
            with cl, TConstructor(list.Objects[k]) do
              begin
                Inc( FMethodCount );
                constr := TConstructor(list.Objects[k]);
                clName := 'TPy'+Copy(cl.Name, 2, Length(cl.Name)-1);
                WriteLn( f, 'function ', cl.Name, '_', GetName, '( Self, Args : PPyObject ) : PPyObject; cdecl; far;' );
                args := FindArgsOfConstructor( constr );
                if Assigned(args) then
                  with args do
                    begin
                      WriteLn( f, 'var' );
                      if not IsGlobalObject( cl.Name ) then
                        WriteLn( f, '  newObj : ', clName, ';' );
                      WriteLn( f, '  _argSelf : PPyObject;' );
                      GenerateMethArgsDecl( f, args as TArgs);
                      WriteLn( f, 'begin' );
                      WriteLn( f, '  Result := nil;' );
                      WriteLn( f, '  with GetPythonEngine do begin' );
                      GenerateMethArgsExtr( f, args as TArgs, True, cl.Name+'.'+constr.GetName );
                      if IsGlobalObject( cl.Name ) then
                        begin
                          WriteLn( f, '    DefInterfaceObject( ', Copy(cl.Name, 2, Length(cl.Name)-1),', _argSelf, False );' );
                        end
                      else
                        begin
                          GenerateMethArgsExpl( f, args as TArgs, True );
                          if CheckMethArgs( args as TArgs) then begin
                            GenerateMethCall( f, args  as TArgs, clName+'.'+GetConstructorName( cl.Name, constr.Name ), 'newObj', False );
                            WriteLn( f, '    DefInterfaceObject( newObj, _argSelf, True );' );
                          end else begin
                            Write( f,   '    //' );
                            GenerateMethCall( f, args  as TArgs, clName+'.'+GetConstructorName( cl.Name, constr.Name ), 'newObj', False );
                            WriteLn( f, '    //DefInterfaceObject( newObj, _argSelf, True );' );
                          end;
                        end;
                      WriteLn( f, '    Result := _argSelf;' );
                      WriteLn( f, '    Py_XIncRef( Result );' );
                      WriteLn( f, '  end;' );
                      WriteLn( f, 'end;' );
                    end
                else
                  begin
                    Inc( FBadMethodCount );
                    Log( 'Could not generate constructor: '+cl.Name+'.'+constr.Name);
                    WriteLn( f, 'begin' );
                    WriteLn( f, '  Result := nil;' );
                    WriteLn( f, '  ErrorMethodNotHandled( ''',cl.Name,'.',constr.Name,''' );' );
                    WriteLn( f, 'end;' );
                  end;
                WriteLn( f, '' );
              end;
      finally
        list.Free;
      end;
    end;

    procedure GenerateReadAttr( var f : TextFile; cl : TMyClass; list : TList );
    var
      i, j : Integer;
      tmp, T : TPascalItem;
      prop : TProperty;
      s : String;
    begin
      for i := 0 to list.Count - 1 do
        begin
          tmp := TPascalItem(list.Items[i]);
          if tmp is TVar then
            with TVar(tmp) do
              for j := 0 to Items.Count - 1 do
                begin
                  Write( f, '    else if CompareText( key, ''', Items.Strings[j],''' ) = 0 then ' );
                  s := FindRootType( VarType );
                  T := TPascalItem(Symbols.Find(s));
                  if IsStrictClass(s) then
                    Write( f, 'Result := GetPythonObject( Self.', Items.Strings[j], ', ''', AUnit.Name, ''', ''', VarType, ''' )' )
                  else if IsSetType(s) then
                    begin
                      WriteLn( f, 'begin' );
                      WriteLn( f, '      tmp', Items.Strings[j], ' := Self.', Items.Strings[j], ';' );
                      WriteLn( f, '      Result := SetToList( @tmp', Items.Strings[j], ', sizeof(tmp', Items.Strings[j],') )' );
                      WriteLn( f, '    end' );
                    end
                  else if IsEnumType(s) or IsInterval(T) then
                    Write( f, 'Result := VariantAsPyObject( Integer( Self.', Items.Strings[j], ' ) )' )
                  else if IsImplementedRecord(s) then
                    Write( f, 'Result := ', MakeRecord( 'Self.'+Items.Strings[j], s ) )
                  else if IsCharType(s)then
                    Write( f, 'Result := VariantAsPyObject( Self.', Items.Strings[j], ' )' )
                  else if IsBasicType(s)then
                    Write( f, 'Result := VariantAsPyObject( Self.', Items.Strings[j], ' )' )
                  else
                    Write( f, 'begin ErrorTypeNotHandled( argSelf, key, ''', VarType, ''' ); Result := nil; end' );
                  WriteLn( f );
                end
          else if tmp is TProperty then
            with TProperty(tmp) do
            begin
              prop := FindOriginalProperty( cl, TProperty(tmp) );
              if not Assigned(prop) then Continue;
              with prop do
                begin
                  if Assigned(Args) then
                    Continue;
                  s := FindRootType( ReturnType );
                  T := TPascalItem(Symbols.Find(s));
                  Write( f, '    else if CompareText( key, ''', Name,''' ) = 0 then ' );
                  if IsStrictClass(s) then
                    Write( f, 'Result := GetPythonObject( Self.', Name, ', ''', AUnit.Name, ''', ''', ReturnType, ''' )' )
                  else if IsSetType(s) then
                    begin
                      WriteLn( f, 'begin' );
                      WriteLn( f, '      tmp', Name, ' := Self.', Name, ';' );
                      WriteLn( f, '      Result := SetToList( @tmp', Name, ', sizeof(tmp', Name,') )' );
                      Write( f, '    end' );
                    end
                  else if IsEnumType(s) or IsInterval(T) then
                    Write( f, 'Result := VariantAsPyObject( Integer( Self.', Name, ' ) )' )
                  else if IsImplementedRecord(s) then
                    Write( f, 'Result := ', MakeRecord( 'Self.'+Name, s ) )
                  else if IsCharType(s)then
                    Write( f, 'Result := VariantAsPyObject( Self.', Name, ' )' )
                  else if IsBasicType(s)then
                    Write( f, 'Result := VariantAsPyObject( Self.', Name, ' )' )
                  else
                    Write( f, 'begin ErrorTypeNotHandled( argSelf, key, ''', ReturnType, ''' ); Result := nil; end' );
                  WriteLn( f );
                end;
            end
          else if tmp is TFunc then
            with TFunc(tmp) do
            begin
            end
          else if tmp is TProc then
            with TProc(tmp) do
            begin
            end;
        end;
    end;

    procedure GenerateWriteAttr( var f : TextFile; cl : TMyClass; list : TList );
    var
      i, j : Integer;
      tmp, T : TPascalItem;
      prop : TProperty;
      s : String;
    begin
      for i := 0 to list.Count - 1 do
        begin
          tmp := TPascalItem(list.Items[i]);
          if tmp is TVar then
            with TVar(tmp) do
              for j := 0 to Items.Count - 1 do
                begin
                  s := FindRootType( VarType );
                  WriteLn( f, ' else if CompareText( key, ''', Items.Strings[j],''' ) = 0 then begin' );
                  if IsStrictClass(s) then
                    begin
                      WriteLn( f, '      obj := GetObjectOf(value);' );
                      WriteLn( f, '      if obj is ', VarType, ' then begin' );
	              WriteLn( f, '        Self.', Items.Strings[j], ' := ', VarType, '( obj );' );
                      WriteLn( f, '        Result := PyInt_FromLong(0);' );
                      Write( f, '      end else ErrorWrongPropertyType( argSelf, obj, ''', Items.Strings[j], ''', ''', VarType, ''' );' );
                    end
                  else if IsSetType(s) then
                    Write( f, '      ErrorTypeNotHandled( argSelf, key, ''', VarType, ''' );' )
                  else if IsEnumType(s) then
                    begin
                      Write( f, '      Self.', Items.Strings[j], ' := ', VarType,'(PyObjectAsVariant( value ));' );
                      Write( f, '      Result := PyInt_FromLong(0);' );
                    end
                  else if IsImplementedRecord(s) then
                    begin
                      Write( f, '      Self.', Items.Strings[j], ' := ', PyObjectAsRecord( s ), ';' );
                      Write( f, '      Result := PyInt_FromLong(0);' );
                    end
                  else if IsCharType(s)then
                    begin
                      WriteLn( f, '      Self.', Items.Strings[j], ' := PyObjectAsChar( value );' );
                      Write(   f, '      Result := PyInt_FromLong(0);' );
                    end
                  else if IsBasicType(s)then
                    begin
                      WriteLn( f, '      Self.', Items.Strings[j], ' := PyObjectAsVariant( value );' );
                      Write(   f, '      Result := PyInt_FromLong(0);' );
                    end
                  else
                    Write( f, '      ErrorTypeNotHandled( argSelf, key, ''', VarType, ''' );' );
                  WriteLn( f );
                  Write( f, '    end' );
                end
          else if tmp is TProperty then
            with TProperty(tmp) do
            begin
              prop := FindOriginalProperty( cl, TProperty(tmp) );
              if not Assigned(prop) then Continue;
              with prop do
                begin
                  if Assigned(Args) then
                    Continue;
                  s := FindRootType( ReturnType );
                  T := TPascalItem(Symbols.Find(s));
                  WriteLn( f, ' else if CompareText( key, ''', Name,''' ) = 0 then begin' );
                  if PropWrite = '' then
                    Write( f, '      ErrorPropReadOnly( argSelf, key );' )
                  else if IsStrictClass(s) then
                    begin
                      WriteLn( f, '      obj := GetObjectOf(value);' );
                      WriteLn( f, '      if obj is ', ReturnType, ' then begin' );
	              WriteLn( f, '        Self.', Name, ' := ', ReturnType, '( obj );' );
                      WriteLn( f, '        Result := PyInt_FromLong(0);' );
                      Write(   f, '      end else ErrorWrongPropertyType( argSelf, obj, ''', Name, ''', ''', ReturnType, ''' );' );
                    end
                  else if IsSetType(s) then
                    begin
                      WriteLn( f, '      if PyList_Check(value) then begin' );
                      WriteLn( f, '        ListToSet( value, @tmp', Name, ', sizeof(tmp', Name,') );' );
                      WriteLn( f, '        Self.', Name, ' := tmp', Name, ';' );
                      WriteLn( f, '        Result := PyInt_FromLong(0);' );
                      WriteLn( f, '      end else' );
                      Write(   f, '        ErrorWrongSetPropertyType( argSelf, ''', Name, ''', ''', ReturnType, ''' );' );
                    end
                  else if IsEnumType(s) or IsInterval(T) then
                    begin
                      //Write( f, '      ErrorTypeNotHandled( argSelf, key, ''', ReturnType, ''' );' )
                      WriteLn( f, '      Self.', Name, ' := ', ReturnType,'(PyObjectAsVariant( value ));' );
                      Write(   f, '      Result := PyInt_FromLong(0);' );
                    end
                  else if IsImplementedRecord(s) then
                    begin
                      Write( f, '      Self.', Name, ' := ', PyObjectAsRecord( s ), ';' );
                      Write( f, '      Result := PyInt_FromLong(0);' );
                    end
                  else if IsCharType(s)then
                    begin
                      WriteLn( f, '      Self.', Name, ' := PyObjectAsChar( value );' );
                      Write(   f, '      Result := PyInt_FromLong(0);' );
                    end
                  else if IsBasicType(s)then
                    begin
                      WriteLn( f, '      Self.', Name, ' := PyObjectAsVariant( value );' );
                      Write(   f, '      Result := PyInt_FromLong(0);' );
                    end
                  else if Assigned(T) and ( (T is TTypeFunc) or (T is TTypeProc)) then
                    begin
                      WriteLn( f, '      if Value = Py_None then begin' );
                      WriteLn( f, '        Self.', Name, ' := nil;' );
                      WriteLn( f, '        Result := AddNewMember( argSelf, key, value );' );
                      WriteLn( f, '      end else if CheckMethod( Value ) then begin' );
                      WriteLn( f, '        if Self is TPy', Copy(cl.Name, 2, Length(cl.Name)-1), ' then' );
                      WriteLn( f, '          with TPy', Copy(cl.Name, 2, Length(cl.Name)-1), '(Self) do' );
                      WriteLn( f, '            ', Name, ' := Event', Name, ';' );
                      WriteLn( f, '        Result := AddNewMember( argSelf, key, value );' );
                      Write( f, '      end else ErrorBadEventType( argSelf, key );' );
                    end
                  else
                    Write( f, '      ErrorTypeNotHandled( argSelf, key, ''', ReturnType, ''' );' );
                  WriteLn( f );
                  Write( f, '    end' );
                end;
            end;
        end;
    end;

    function GenerateSetPropDecl( var f : TextFile; cl : TMyClass; list : TList; defVar : Boolean ) : Boolean;
    var
      i : Integer;
      tmp : TPascalItem;
      prop : TProperty;
      s : String;
      defined : Boolean;
    begin
      defined := False;
      for i := 0 to List.Count - 1 do
        begin
          tmp := TPascalItem(list.Items[i]);
          if tmp is TProperty then
            with TProperty(tmp) do
            begin
              prop := FindOriginalProperty( cl, TProperty(tmp) );
              if not Assigned(prop) then Continue;
              with prop do
                begin
                  s := FindRootType( ReturnType );
                  if IsSetType(s) then
                    begin
                      if defVar and not defined then
                        begin
                          defined := True;
                          WriteLn( f, 'var' );
                        end;
                      WriteLn( f, '  tmp', Name, ' : ', ReturnType, ';' );
                    end;
                end;
            end;
        end;
      Result := not defined;
    end;

    function GetUnitRef( const symb : String ) : String;
    //var
    //  aUnit : TUnit;
    begin
      {AUnit := FindUnit(symb);
      if Assigned(AUnit) then
        Result := AUnit.Name + '.'
      else
        Result := '';}
      Result := '';
    end;

    procedure GenerateConstructorBinding( var f : TextFile; cl : TMyClass );
    var
      i : Integer;
      s : String;
      list : TStringList;
    begin
      list := TStringList.Create;
      try
        FindConstructors( cl, list );
        s := Copy(cl.Name, 2, Length(cl.Name)-1);
        for i := 0 to list.Count - 1 do
          if list.Objects[i] is TConstructor then
            with TConstructor(list.Objects[i]) do
              begin
                WriteLn( f, '  AddMethod( ''', GetName,'', s, ''', ', cl.Name, '_', Name,', '''' );' );
              end;
      finally
        list.Free;
      end;
    end;

    procedure GenerateMethodsWrappers( var f : TextFile; cl : TMyClass; list : TList; const UnitRef : String );

      procedure GenMethod( args : TArgs; const methName, originalName, ReturnType : String; isProperty : Boolean );

        function CheckArgs : Boolean;
        var
          i : Integer;
        begin
          Result := True;
          for i := 0 to args.Items.Count - 1 do
            with TArg(args.Items[i]) do begin
              if IsArrayOf or
                 IsVar or
                 ((IsConst or IsOut) and (Name = '')) then
                begin
                  Result := False;
                  Break;
                end;
            end;
        end;

        function MethodAsString : String;
        begin
          if ReturnType = '' then
            Result := Format('procedure %s.%s%s', [cl.Name, originalName, args.AsString])
          else
            Result := Format('function %s.%s%s : %s', [cl.Name, originalName, args.AsString, ReturnType]);
        end;

      var
        args_ok : Boolean;
      begin
        Inc( FMethodCount );
        FillListsWithArgs( args );
        WriteLn( f, 'function ', cl.Name, '_', methName, '( Self, Args : PPyObject ) : PPyObject; cdecl;' );
        args_ok := CheckArgs and CheckMethArgs( args );
        if args_ok then begin
          WriteLn( f, 'var' );
          WriteLn( f, '  obj : TObject;' );
          WriteLn( f, '  realObj : ', UnitRef, cl.Name, ';' );
          if ReturnType <> '' then
            WriteLn( f, '  rslt : ', ReturnType, ';' );
          GenerateMethArgsDecl( f, args );
        end else begin
          Log( 'Could not generate method: '+MethodAsString );
          Inc( FBadMethodCount );
        end;
        WriteLn( f, 'begin' );
        WriteLn( f, '  Result := nil;' );
        WriteLn( f, '  with GetPythonEngine do begin' );
        if args_ok then begin
          WriteLn( f, '    // Parsing of method args' );
          GenerateMethArgsExtr( f, args, False, cl.Name+'.'+methName );
          GenerateMethArgsExpl( f, args, False );
          WriteLn( f, '    // Check Delphi object' );
          WriteLn( f, '    obj := GetObjectOf( Self );' );
          WriteLn( f, '    if not Assigned(obj) then begin' );
          WriteLn( f, '      ErrorNoObject( Self );' );
          WriteLn( f, '      Exit;' );
          WriteLn( f, '    end;' );
          WriteLn( f, '    // Check object type' );
          WriteLn( f, '    if not( obj is ', UnitRef, cl.Name, ') then begin' );
          WriteLn( f, '      ErrorWrongExpectedType( Self, ''', cl.Name, ''' );' );
          WriteLn( f, '      Exit;' );
          WriteLn( f, '    end;' );
          WriteLn( f, '    realObj := ', UnitRef, cl.Name, '(obj);' );
          if ReturnType <> '' then
            begin
              GenerateMethCall( f, args, 'realObj.'+originalName, 'rslt', isProperty );
              GenerateResultCall( f, args, cl.Name+'.'+originalName, 'rslt', ReturnType, AUnit.Name );
            end
          else
            begin
              GenerateMethCall( f, args, 'realObj.'+originalName, '', False );
              WriteLn( f, '    Result := ReturnNone;' );
            end;
        end
        else
          WriteLn( f, '    Result := ReturnNone;' );
        WriteLn( f, '  end;' );
        WriteLn( f, 'end;' );
        WriteLn( f, '' );
      end;

    var
      i : Integer;
    begin
      for i := 0 to list.Count - 1 do
        if (TPascalItem(list.Items[i]) is TFunc) then
          with TFunc(list.Items[i]) do
            GenMethod( Args as TArgs, GetName, Name, ReturnType, False )
        else if (TPascalItem(list.Items[i]) is TProc) then
          with TProc(list.Items[i]) do
            GenMethod( Args as TArgs, GetName, Name, '', False )
        else if (TPascalItem(list.Items[i]) is TProperty) and (TProperty(list.Items[i]).Args <> nil) then
          with TProperty(list.Items[i]) do
            GenMethod( Args as TArgs, Name, Name, ReturnType, True );
    end;

    procedure GenerateMethodsBinding( var f : TextFile; cl : TMyClass; list : TList );

      function IsMethod( item : TPascalItem ) : Boolean;
      begin
        Result := (item is TMethod) or
                  ((item is TProperty) and (TProperty(item).Args <> nil));
      end;

    var
      i : Integer;
    begin
      for i := 0 to list.Count - 1 do
        if IsMethod(TPascalItem(list.Items[i])) then
           with TPascalItem(list.Items[i]) do
             WriteLn( f, '  g', cl.Name,'.AddMethod( ''', GetName, ''', ', cl.Name, '_', Name,', '''' );' );
    end;

  var
    f : TextFile;
    i : Integer;
    s, parentClass : String;
    unitRef : String;
    empty : Boolean;
  begin
    AssignFile( f, AppendSlash(Settings.edDelphiFiles.Text)+'Python_'+AUnit.Name+'.pas' );
    Rewrite( f );
    with AUnit do
    try
      // Write banner
      WriteLn( f, '////' );
      WriteLn( f, '//  This file was generated by VCL Generator' );
      WriteLn( f, '//  Copyright 1998 - Morgan Martinet' );
      WriteLn( f, '//  ', DateTimeToStr(Now) );
      WriteLn( f, '//  it declares the content of the module _', AUnit.Name );
      WriteLn( f, '////' );
      // Generate unit name
      WriteLn( f, '' );
      WriteLn( f, 'unit Python_', AUnit.Name, ';' );
      WriteLn( f, '' );
      WriteLn( f, 'interface' );
      WriteLn( f, '' );
      // Generate uses
      WriteLn( f, 'uses' );
      WriteLn( f, '   Windows,' );
      for i := 0 to UsesList.Count - 1 do
        if IsUnitPresent( UsesList.Strings[i] ) and
           (CompareText( UsesList.Strings[i], 'Classes' ) <> 0) and
           (CompareText( UsesList.Strings[i], 'SysUtils' ) <> 0) and
           (CompareText( UsesList.Strings[i], 'Windows' ) <> 0) then
          begin
            WriteLn( f, '   ', UsesList.Strings[i], ',' );
          end;
      if (CompareText( Name, 'Classes' ) <> 0) and
         (CompareText( Name, 'System' ) <> 0) and
         (CompareText( Name, 'SysUtils' ) <> 0) and
         (CompareText( Name, 'Windows' ) <> 0) then
        WriteLn( f, '   ', Name, ',' );
      if UsesList.IndexOf( 'ActiveX' ) >= 0 then
        WriteLn( f, '   ActiveX,' );
      if UsesList.IndexOf( 'Messages' ) >= 0 then
        WriteLn( f, '   Messages,' );
      WriteLn( f, '   Classes,' );
      WriteLn( f, '   SysUtils,' );
      WriteLn( f, '   PythonEngine;' );
      WriteLn( f, '' );
      // Generate Module declaration
      WriteLn( f, 'type' );
      WriteLn( f, '  TModule_', AUnit.Name, ' = class( TPythonModule )' );
      WriteLn( f, '  public' );
      WriteLn( f, '    procedure Initialize; override;' );
      WriteLn( f, '  end;' );
      WriteLn( f, '' );
      // Generate class functions prototypes
      for i := 0 to TypeCount - 1 do
        if (Types[i] is TMyClass) and not (Types[i] is TMyInterface) then
          with TMyClass(Types[i]) do
            begin
              // We skip the excluded symbols
              if IsSymbolExcluded( Name ) then
                Continue;
              // We skip the exceptions (classes that begins with E)
              if Name[1] = 'E' then
                Continue;
              WriteLn( f, '  function ', Name, '_ReadAttribute( Self : ', GetUnitRef(Name), Name, '; argSelf : PPyObject; key : PChar ) : PPyObject;' );
              WriteLn( f, '  function ', Name, '_WriteAttribute( Self : ', GetUnitRef(Name), Name, '; argSelf : PPyObject; key : PChar; value : PPyObject ) : PPyObject;' );
            end;
      WriteLn( f, '' );
      WriteLn( f, '  procedure Init( AOwner : TComponent; AEngine : TPythonEngine ); ' );
      WriteLn( f, '' );
      // Generate implementation
      WriteLn( f, 'implementation' );
      WriteLn( f, '' );
      // Generate uses
      WriteLn( f, 'uses' );
      for i := 0 to UsesList.Count - 1 do
        if IsUnitPresent( UsesList.Strings[i] ) then
          begin
            WriteLn( f, '   Delphi_', UsesList.Strings[i], ',' );
            if CompareText( Name, UsesList.Strings[i] ) <> 0 then
              WriteLn( f, '   Python_', UsesList.Strings[i], ',' );
          end;
      WriteLn( f, '   Delphi_System,' );
      if (CompareText( Name, 'System' ) <> 0) then
        begin
          WriteLn( f, '   Delphi_', Name, ',' );
          WriteLn( f, '   Python_System,' );
        end;
      WriteLn( f, '   Py_Misc,' );
      WriteLn( f, '   PyRecords,' );
      WriteLn( f, '   PyDelphiAssoc;' );
      WriteLn( f, '' );
      // Generate init proc
      WriteLn( f, 'var' );
      WriteLn( f, '  IsInitialized : Boolean;' );
      WriteLn( f, '  g', AUnit.Name, ' : TModule_', AUnit.Name, ';' );
      for i := 0 to TypeCount - 1 do
        if (Types[i] is TMyClass) and not (Types[i] is TMyInterface) then
          with TMyClass(Types[i]) do
            if not IsSymbolExcluded( Name ) then
              WriteLn( f, '  g', Name, ' : TMethodsContainer;' );
      WriteLn( f, '' );
      WriteLn( f, 'procedure Init( AOwner : TComponent; AEngine : TPythonEngine ); ' );
      WriteLn( f, 'begin' );
      WriteLn( f, '  if IsInitialized then' );
      WriteLn( f, '    Exit;' );
      WriteLn( f, '  IsInitialized := True;' );
      WriteLn( f, '  g', AUnit.Name, ' := TModule_', AUnit.Name, '.Create( AOwner );' );
      WriteLn( f, '  with g', AUnit.Name, ' do' );
      WriteLn( f, '    begin' );
      WriteLn( f, '      Engine := AEngine;' );
      WriteLn( f, '      ModuleName := ''_', AUnit.Name, ''';'  );
      WriteLn( f, '    end;' );
      for i := 0 to TypeCount - 1 do
        if (Types[i] is TMyClass) and not (Types[i] is TMyInterface) then
          with TMyClass(Types[i]) do
            if not IsSymbolExcluded( Name ) then
              WriteLn( f, '  g', Name, ' := TMethodsContainer.Create( AOwner );' );
      WriteLn( f, 'end;' );
      WriteLn( f, '' );
      // Generate class functions
      for i := 0 to TypeCount - 1 do
        if (Types[i] is TMyClass) and not (Types[i] is TMyInterface) then
          with TMyClass(Types[i]) do
            begin
              // We skip the excluded symbols
              if IsSymbolExcluded( Name ) then
                Continue;
              // We skip the exceptions (classes that begins with E)
              if Name[1] = 'E' then
                Continue;
              WriteLn( f, '///////////// Functions for class ', Name, ' //////////////////' );
              WriteLn( f, '' );
              // Write constructor
              GenerateClassConstructor( f, TMyClass(Types[i]) );
              parentClass := GetParentClass( TMyClass(Types[i]) );
              UnitRef := GetUnitRef(Name);

              // Write read attribute

              // Write interface function that does the extraction and the checking
              WriteLn( f, 'function ', Name, '_GetAttr( Self, Args : PPyObject ) : PPyObject; cdecl; far;' );
              WriteLn( f, 'var' );
              WriteLn( f, '  argSelf : PPyObject;' );
              WriteLn( f, '  argKey : PChar;' );
              WriteLn( f, '  obj : TObject;' );
              WriteLn( f, '  realObj : ', UnitRef, Name, ';' );
              WriteLn( f, 'begin' );
              WriteLn( f, '  Result := nil;' );
              WriteLn( f, '  with GetPythonEngine do begin' );
              WriteLn( f, '    // Extract args' );
              WriteLn( f, '    if PyArg_ParseTuple( Args, ''Os'', [@argSelf, @argKey]) = 0 then' );
              WriteLn( f, '      Exit;' );
              WriteLn( f, '    // Check Delphi object' );
              WriteLn( f, '    obj := GetObjectOf( argSelf );' );
              WriteLn( f, '    if not Assigned(obj) then begin' );
              WriteLn( f, '      ErrorNoObject( argSelf );' );
              WriteLn( f, '      Exit;' );
              WriteLn( f, '    end;' );
              WriteLn( f, '    // Check object type' );
              WriteLn( f, '    if not( obj is ', UnitRef, Name, ') then begin' );
              WriteLn( f, '      ErrorWrongExpectedType( argSelf, ''', Name, ''' );' );
              WriteLn( f, '      Exit;' );
              WriteLn( f, '    end;' );
              WriteLn( f, '    realObj := ', UnitRef, Name, '(obj);' );
              WriteLn( f, '    // Check attribute' );
              WriteLn( f, '    Result := ', Name, '_ReadAttribute( realObj, argSelf, argKey );' );
              WriteLn( f, '    // Check result' );
              WriteLn( f, '    if not Assigned(Result) and (PyErr_Occurred = nil) then' );
              WriteLn( f, '      UnknownProperty( argSelf, argKey );' );
              WriteLn( f, '  end;' );
              WriteLn( f, 'end;' );
              WriteLn( f, '' );

              // Write the function that really finds the value of an attribute
              WriteLn( f, 'function ', Name, '_ReadAttribute( Self : ', UnitRef, Name, '; argSelf : PPyObject; key : PChar ) : PPyObject;' );
              empty := GenerateSetPropDecl( f, TMyClass(Types[i]), PublicList, True );
              GenerateSetPropDecl( f, TMyClass(Types[i]), PublishedList, empty );
              WriteLn( f, 'begin' );
              WriteLn( f, '  with GetPythonEngine do begin' );
              WriteLn( f, '    if g', Name, '.MethodCount > 0 then ' );
              WriteLn( f, '      Result := Py_FindMethod( g', Name, '.MethodsData, argSelf, key)' );
              WriteLn( f, '    else Result := nil;' );
              WriteLn( f, '    if Assigned(Result) then Exit else PyErr_Clear;' );
              WriteLn( f, '    if CompareText( key, ''Properties'') = 0 then Result := GetPropList( Self )' );
              GenerateReadAttr( f, TMyClass(Types[i]), PublicList );
              GenerateReadAttr( f, TMyClass(Types[i]), PublishedList );
              if (CompareText( Name, 'TObject' ) = 0) or (parentClass = '') then
                WriteLn( f, '    else Result := nil;' )
              else
                WriteLn( f, '    else Result := ', parentClass, '_ReadAttribute( Self, argSelf, key );' );
              WriteLn( f, '  end;' );
              WriteLn( f, 'end;' );
              WriteLn( f, '' );

              // Write set attribute

              // Write interface function that does the extraction and the checking
              WriteLn( f, 'function ', Name, '_SetAttr( Self, Args : PPyObject ) : PPyObject; cdecl; far;' );
              WriteLn( f, 'var' );
              WriteLn( f, '  argSelf, argValue : PPyObject;' );
              WriteLn( f, '  argKey : PChar;' );
              WriteLn( f, '  obj : TObject;' );
              WriteLn( f, '  realObj : ', UnitRef, Name, ';' );
              WriteLn( f, 'begin' );
              WriteLn( f, '  Result := nil;' );
              WriteLn( f, '  with GetPythonEngine do begin' );
              WriteLn( f, '    // Extract args' );
              WriteLn( f, '    if PyArg_ParseTuple( Args, ''OsO'', [@argSelf, @argKey, @argValue]) = 0 then' );
              WriteLn( f, '      Exit;' );
              WriteLn( f, '    // Check Delphi object' );
              WriteLn( f, '    obj := GetObjectOf( argSelf );' );
              WriteLn( f, '    if not Assigned(obj) then begin' );
              WriteLn( f, '      ErrorNoObject( argSelf );' );
              WriteLn( f, '      Exit;' );
              WriteLn( f, '    end;' );
              WriteLn( f, '    // Check object type' );
              WriteLn( f, '    if not( obj is ', UnitRef, Name, ') then begin' );
              WriteLn( f, '      ErrorWrongExpectedType( argSelf, ''', Name, ''' );' );
              WriteLn( f, '      Exit;' );
              WriteLn( f, '    end;' );
              WriteLn( f, '    realObj := ', UnitRef, Name, '(obj);' );
              WriteLn( f, '    // Check attribute' );
              WriteLn( f, '    Result := ', Name, '_WriteAttribute( realObj, argSelf, argKey, argValue );' );
              WriteLn( f, '    // Check result' );
              WriteLn( f, '    if not Assigned(Result) and (PyErr_Occurred = nil) then' );
              WriteLn( f, '      Result := AddNewMember( argSelf, argKey, argValue );' );
              WriteLn( f, '  end;' );
              WriteLn( f, 'end;' );
              WriteLn( f, '' );

              // Write the function that really sets the value of an attribute
              WriteLn( f, 'function ', Name, '_WriteAttribute( Self : ', UnitRef, Name, '; argSelf : PPyObject; key : PChar; value : PPyObject ) : PPyObject;' );
              WriteLn( f, 'var' );
              WriteLn( f, '  obj : TObject;' );
              GenerateSetPropDecl( f, TMyClass(Types[i]), PublicList, False );
              GenerateSetPropDecl( f, TMyClass(Types[i]), PublishedList, False );
              WriteLn( f, 'begin' );
              WriteLn( f, '  Result := nil;' );
              WriteLn( f, '  obj := nil;' );
              WriteLn( f, '  with GetPythonEngine do begin' );
              WriteLn( f, '    if False then begin' );
              Write(   f, '    end' );
              GenerateWriteAttr( f, TMyClass(Types[i]), PublicList );
              GenerateWriteAttr( f, TMyClass(Types[i]), PublishedList );
              if (CompareText( Name, 'TObject' ) = 0) or (parentClass = '') then
                WriteLn( f, ';' )
              else
                WriteLn( f, ' else Result := ', parentClass, '_WriteAttribute( Self, argSelf, key, value );' );
              WriteLn( f, '  end;' );
              WriteLn( f, 'end;' );
              WriteLn( f, '' );

              // Write methods wrappers
              GenerateMethodsWrappers( f, TMyClass(Types[i]), PublicList, UnitRef );
              GenerateMethodsWrappers( f, TMyClass(Types[i]), PublishedList, UnitRef );
            end;

      // Generate initialization of the module
      WriteLn( f, '///////////// Initialization of the module ///////////' );
      WriteLn( f, '' );
      WriteLn( f, 'procedure TModule_', AUnit.Name, '.Initialize;' );
      WriteLn( f, 'begin' );
      // Generate importation of the class functions
      for i := 0 to TypeCount - 1 do
        if (Types[i] is TMyClass) and not (Types[i] is TMyInterface) then
          with TMyClass(Types[i]) do
            begin
              // We skip the excluded symbols
              if IsSymbolExcluded( Name ) then
                Continue;
              // We skip the exceptions (classes that begins with E)
              if Name[1] = 'E' then
                Continue;
              s := Copy(Name, 2, Length(Name)-1);
              // Write importation of the class functions
              WriteLn( f, '  // class ', Name );
              GenerateConstructorBinding( f, TMyClass(Types[i]) );
              WriteLn( f, '  AddMethod( ''', s, '_GetAttr'', ', Name, '_GetAttr, '''' );' );
              WriteLn( f, '  AddMethod( ''', s, '_SetAttr'', ', Name, '_SetAttr, '''' );' );
              GenerateMethodsBinding( f, TMyClass(Types[i]), PublicList );
              GenerateMethodsBinding( f, TMyClass(Types[i]), PublishedList );
            end;
      WriteLn( f, '  inherited;' );
      WriteLn( f, 'end;' );
      WriteLn( f, '' );
      // Generate end of unit
      WriteLn( f, 'end.' );
    finally
      CloseFile( f );
    end;
  end;

  procedure GenerateDelphiMapping;

    procedure GenerateEventHandlersProto( var f : TextFile; cl : TMyClass; list : TList );
    var
      i : Integer;
      tmp, T : TPascalItem;
      prop : TProperty;
      s : String;
    begin
      for i := 0 to list.Count - 1 do
        begin
          tmp := TPascalItem(list.Items[i]);
          if tmp is TProperty then
            //with TProperty(tmp) do
            begin
              prop := FindOriginalProperty( cl, TProperty(tmp) );
              if not Assigned(prop) then Continue;
              with prop do
                begin
                  if Assigned(Args) then
                    Continue;
                  s := FindRootType( ReturnType );
                  T := TPascalItem(Symbols.Find(s));
                  if not Assigned(T) then
                    Continue;
                  if T is TTypeFunc then
                    with TTypeFunc(T) do
                      begin
                        if not IsOfObject then
                          Continue;
                        WriteLn( f, '    function  Event', prop.Name, Args.AsString, ' : ', ReturnType, ';' );
                      end
                  else if T is TTypeProc then
                    with TTypeProc(T) do
                      begin
                        if not IsOfObject then
                          Continue;
                        WriteLn( f, '    procedure Event', prop.Name, Args.AsString, ';' );
                      end;
                end; // with prop
            end; // if
        end; // for
    end; // procedure

    procedure GenerateEventHandlersBody( var f : TextFile; cl : TMyClass; list : TList; const NewClass : String);
    var
      i, j : Integer;
      tmp, T : TPascalItem;
      prop : TProperty;
      s, s2 : String;

      function ConvertArg : String;
      begin
        if IsStrictClass( s ) then
          Result := 'GetPythonObject( ' + FList1.Strings[j] + ', ''' + cl.RefUnit.Name + ''', ''' + cl.Name + ''' )'
        else if IsSetType( s ) then
          Result := 'SetToList( @' + FList1.Strings[j] + ', sizeof(' + FList1.Strings[j] + ') )'
        else if IsEnumType( s ) or IsInterval(T) then
          Result := 'GetPythonEngine.PyInt_FromLong( Integer(' + FList1.Strings[j] + ') )'
        else if IsImplementedRecord( s ) then
          Result := MakeRecord( FList1.Strings[j], s)
        else if IsCharType( s ) then
          Result := 'GetPythonEngine.VariantAsPyObject( ' + FList1.Strings[j] + ' )'
        else if IsBasicType( s ) then
          Result := 'GetPythonEngine.VariantAsPyObject( ' + FList1.Strings[j] + ' )'
        else
          Result := 'GetPythonEngine.Py_None';
      end;

      function ConvertArgBack : String;
      var
        tmp : String;
      begin
        tmp := Format('ExtractValueOfVarArg( PPyObject(L.Items[%d]) )', [j]);
        if IsStrictClass( s ) then
          Result := 'GetObjectOf( ' + tmp + ' ) as ' + FList2.Strings[j]
        else if IsSetType( s ) then
          Result := 'ListToSet( ' + tmp + ', @' + FList1.Strings[j] + ', sizeof(' + FList1.Strings[j] + ') )'
        else if IsEnumType( s ) or IsInterval(T) then
          Result := FList2.Strings[j]+'( GetPythonEngine.PyObjectAsVariant( ' + tmp + ' ) )'
        else if IsImplementedRecord( s ) then
          Result := PyObjectAsRecordWith( s, tmp )
        else if IsCharType( s ) then
          Result := 'PyObjectAsChar( ' + tmp + ' )'
        else if IsBasicType( s ) then
          Result := 'GetPythonEngine.PyObjectAsVariant( ' + tmp + ' )'
        else
          Result := '';
      end;

    begin
      for i := 0 to list.Count - 1 do
        begin
          tmp := TPascalItem(list.Items[i]);
          if tmp is TProperty then
            begin
              prop := FindOriginalProperty( cl, TProperty(tmp) );
              if not Assigned(prop) then Continue;
              with prop do
                begin
                  if Assigned(Args) then
                    Continue;
                  s := FindRootType( ReturnType );
                  T := TPascalItem(Symbols.Find(s));
                  if not Assigned(T) then
                    Continue;
                  if T is TTypeFunc then
                    with TTypeFunc(T) do
                      begin
                        if not IsOfObject then
                          Continue;
                        FillListsWithArgs( Args as TArgs);
                        WriteLn( f, 'function ', NewClass, '.Event', prop.Name, Args.AsString, ' : ', ReturnType, ';' );
                        WriteLn( f, 'var' );
                        WriteLn( f, '  args, rslt : PPyObject;' );
                        WriteLn( f, '  L : TList;' );
                        WriteLn( f, 'begin' );
                        WriteLn( f, '  L := TList.Create;' );
                        WriteLn( f, '  try' );
                        for j := 0 to FList1.Count - 1 do
                          begin
                            s := FindRootType( FList2.Strings[j] );
                            if FList3.Strings[j] = 'var' then
                              WriteLn( f, '    L.Add( CreateVarArg( ', ConvertArg, ' ).GetSelf ); ' )
                            else
                              WriteLn( f, '    L.Add( ', ConvertArg, ' ); ' );
                          end;
                        WriteLn( f, '    rslt := ExecuteEvent( ''', prop.Name, ''', TDelphiAssoc(__assoc__), L, args );' );
                        for j := 0 to FList1.Count - 1 do
                          begin
                            if FList3.Strings[j] = 'var' then
                              begin
                                s := FindRootType( FList2.Strings[j] );
                                s2 := ConvertArgBack;
                                if s2 <> '' then
                                  WriteLn( f, '    ', FList1.Strings[j], ' := ', s2, ';' );
                              end;
                          end;
                        if IsBasicType(ReturnType) then
                          begin
                            WriteLn( f, '    Result := GetPythonEngine.PyObjectAsVariant( rslt );' );
                          end;
                        WriteLn( f, '    GetPythonEngine.Py_XDecRef( rslt );' );
                        WriteLn( f, '    GetPythonEngine.Py_XDecRef( args );' );
                        WriteLn( f, '  finally' );
                        WriteLn( f, '    L.Free;' );
                        WriteLn( f, '  end;' );
                        WriteLn( f, 'end;' );
                        WriteLn( f, '' );
                      end
                  else if T is TTypeProc then
                    with TTypeProc(T) do
                      begin
                        if not IsOfObject then
                          Continue;
                        FillListsWithArgs( Args as TArgs);
                        WriteLn( f, 'procedure ', NewClass, '.Event', prop.Name, Args.AsString, ';' );
                        WriteLn( f, 'var' );
                        WriteLn( f, '  args, rslt : PPyObject;' );
                        WriteLn( f, '  L : TList;' );
                        WriteLn( f, 'begin' );
                        WriteLn( f, '  L := TList.Create;' );
                        WriteLn( f, '  try' );
                        for j := 0 to FList1.Count - 1 do
                          begin
                            s := FindRootType( FList2.Strings[j] );
                            if FList3.Strings[j] = 'var' then
                              WriteLn( f, '    L.Add( CreateVarArg( ', ConvertArg, ' ).GetSelf ); ' )
                            else
                              WriteLn( f, '    L.Add( ', ConvertArg, ' ); ' );
                          end;
                        WriteLn( f, '    rslt := ExecuteEvent( ''', prop.Name, ''', TDelphiAssoc(__assoc__), L, args );' );
                        for j := 0 to FList1.Count - 1 do
                          begin
                            if FList3.Strings[j] = 'var' then
                              begin
                                s := FindRootType( FList2.Strings[j] );
                                s2 := ConvertArgBack;
                                if s2 <> '' then
                                  WriteLn( f, '    ', FList1.Strings[j], ' := ', s2, ';' );
                              end;
                          end;
                        WriteLn( f, '    GetPythonEngine.Py_XDecRef( rslt );' );
                        WriteLn( f, '    GetPythonEngine.Py_XDecRef( args );' );
                        WriteLn( f, '  finally' );
                        WriteLn( f, '    L.Free;' );
                        WriteLn( f, '  end;' );
                        WriteLn( f, 'end;' );
                        WriteLn( f, '' );
                      end;
                end; // with prop
            end; // if
        end; // for
    end; // procedure

  var
    f : TextFile;
    i : Integer;
    s : String;
  begin
    AssignFile( f, AppendSlash(Settings.edDelphiFiles.Text)+'Delphi_'+AUnit.Name+'.pas' );
    Rewrite( f );
    with AUnit do
    try
      // Write banner
      WriteLn( f, '////' );
      WriteLn( f, '//  This file was generated by VCL Generator' );
      WriteLn( f, '//  Copyright 1998 - Morgan Martinet' );
      WriteLn( f, '//  ', DateTimeToStr(Now) );
      WriteLn( f, '//  it subclasses all classes of the unit ', AUnit.Name );
      WriteLn( f, '////' );
      // Generate unit name
      WriteLn( f, '' );
      WriteLn( f, 'unit Delphi_', AUnit.Name, ';' );
      WriteLn( f, '' );
      WriteLn( f, 'interface' );
      WriteLn( f, '' );
      // Generate uses
      WriteLn( f, 'uses' );
      for i := 0 to UsesList.Count - 1 do
        if CompareText( UsesList.Strings[i], 'System' ) <> 0 then
          WriteLn( f, '  ', UsesList.Strings[i], ',' );
      if CompareText( Name, 'System' ) <> 0 then
        WriteLn( f, '  ', Name, ',' );
      WriteLn( f, '  PythonEngine,' );
      WriteLn( f, '  PyVarArg,' );
      WriteLn( f, '  PyRecords,' );
      WriteLn( f, '  PyDelphiAssoc;' );
      WriteLn( f, '' );
      WriteLn( f, 'type' );
      // Generate class subclassing
      for i := 0 to TypeCount - 1 do
        if (Types[i] is TMyClass) and not (Types[i] is TMyInterface) then
          with TMyClass(Types[i]) do
            begin
              // We skip the excluded symbols
              if IsSymbolExcluded( Name ) then
                Continue;
              // We skip the exceptions (classes that begins with E)
              if Name[1] = 'E' then
                Continue;
              s := 'TPy'+Copy(Name, 2, Length(Name)-1);
              WriteLn( f, '  ', s, ' = class( ', Name, ' )' );
              WriteLn( f, '  protected' );
              WriteLn( f, '    FAssoc : Integer;' );
              WriteLn( f, '  public' );
              WriteLn( f, '    destructor Destroy; override;' );
              // Define event handlers
              GenerateEventHandlersProto( f, TMyClass(Types[i]), PublicList );
              GenerateEventHandlersProto( f, TMyClass(Types[i]), PublishedList );
              // Define property __assoc__
              WriteLn( f, '  published' );
              WriteLn( f, '    property __assoc__ : Integer read FAssoc write FAssoc;' );
              WriteLn( f, '  end;' );
              WriteLn( f, '' );
            end;
      WriteLn( f, '' );
      // Generate implementation
      WriteLn( f, 'implementation' );
      WriteLn( f, '' );
      WriteLn( f, 'Uses Py_Misc;' );
      WriteLn( f, '' );
      for i := 0 to TypeCount - 1 do
        if (Types[i] is TMyClass) and not (Types[i] is TMyInterface) then
          with TMyClass(Types[i]) do
            begin
              // We skip the excluded symbols
              if IsSymbolExcluded( Name ) then
                Continue;
              // We skip the exceptions (classes that begins with E)
              if Name[1] = 'E' then
                Continue;
              s := 'TPy'+Copy(Name, 2, Length(Name)-1);
              WriteLn( f, '/////////// class ', s, ' /////////////////////' );
              WriteLn( f, '' );
              WriteLn( f, 'destructor ', s, '.Destroy;' );
              WriteLn( f, 'begin' );
              WriteLn( f, '  ClearInterface( TDelphiAssoc(FAssoc) );' );
              WriteLn( f, '  FAssoc := 0;' );
              WriteLn( f, '  inherited;' );
              WriteLn( f, 'end;' );
              WriteLn( f, '' );
              // Define event handlers
              GenerateEventHandlersBody( f, TMyClass(Types[i]), PublicList, s );
              GenerateEventHandlersBody( f, TMyClass(Types[i]), PublishedList, s );
            end;
      WriteLn( f, '' );
      // Generate end of unit
      WriteLn( f, 'end.' );
    finally
      CloseFile( f );
    end;
  end;

begin
  StatusBar1.SimpleText := Format('Generating unit %s',[AUnit.Name]);
  Log( '##################################################################' );
  Log( StatusBar1.SimpleText );
  GeneratePythonFile;
  GeneratePythonInterface;
  GenerateDelphiMapping;
end;

procedure TMainForm.Settings1Click(Sender: TObject);
begin
  Settings.ShowModal;
end;

procedure TMainForm.BuildSymbolsOf( AUnit : TUnit );
var
  i : Integer;
begin
  with AUnit do
    for i := 0 to TypeCount - 1 do
      with Types[i] do
        Symbols.Add( Name, Types[i] );
end;

function TMainForm.IsClass( const Name : String ) : Boolean;
var
  tmp : TPascalItem;
begin
  tmp := TPascalItem(Symbols.Find( Name ));
  Result := Assigned(tmp) and (tmp is TMyClass);
end;

function TMainForm.IsStrictClass( const Name : String ) : Boolean;
var
  tmp : TPascalItem;
begin
  tmp := TPascalItem(Symbols.Find( Name ));
  Result := Assigned(tmp) and (tmp is TMyClass) and not (tmp is TMyInterface);
end;

function TMainForm.IsIntType( const val : String ) : Boolean;
var
  s : String;
begin
  s := UpperCase(val);
  Result := (s = 'INTEGER') or
            (s = 'LONGINT') or
            (s = 'SHORTINT') or
            (s = 'SMALLINT') or
            (s = 'BYTE') or
            (s = 'WORD') or
            (s = 'LONGWORD') or
            (s = 'THANDLE') or
            (s = 'HDC') or
            (s = 'HWND') or
            (s = 'HPALETTE') or
            (s = 'HBITMAP') or
            (s = 'HCURSOR') or
            (s = 'HICON') or
            (s = 'HRESULT') or
            (s = 'HCURSOR') or
            (s = 'CARDINAL');
end;

function TMainForm.IsRealType( const val : String ) : Boolean;
var
  s : String;
begin
  s := UpperCase(val);
  Result := (s = 'REAL') or
            (s = 'SINGLE') or
            (s = 'DOUBLE') or
            (s = 'EXTENDED') or
            (s = 'COMP') or
            (s = 'CURRENCY');
end;

function TMainForm.IsStringType( const val : String ) : Boolean;
var
  s : String;
begin
  s := UpperCase(val);
  Result := (s = 'STRING') or
            (s = 'SHORTSTRING') or
            (s = 'WIDESTRING') or
            (s = 'PCHAR');
end;

function TMainForm.IsCharType( const val : String ) : Boolean;
var
  s : String;
begin
  s := UpperCase(val);
  Result := (s = 'CHAR');
end;

function TMainForm.IsBooleanType( const val : String ) : Boolean;
var
  s : String;
begin
  s := UpperCase(val);
  Result := (s = 'BOOLEAN') or
            (s = 'BYTEBOOL') or
            (s = 'WORDBOOL') or
            (s = 'LONGBOOL');
end;

function TMainForm.IsEnumType( const val : String ) : Boolean;
var
  tmp : TPascalItem;
begin
  tmp := TPascalItem(Symbols.Find( val ));
  Result := Assigned(tmp) and (tmp is TEnum);
end;

function TMainForm.IsSetType( const val : String ) : Boolean;
var
  tmp : TPascalItem;
begin
  tmp := TPascalItem(Symbols.Find( val ));
  Result := Assigned(tmp) and (tmp is TSet);
end;


function TMainForm.IsBasicType( const val : String ) : Boolean;
begin
  Result := IsIntType(val) or
            IsRealType(val) or
            IsStringType(val) or
            IsBooleanType(val) or
            IsEnumType(val) or
            IsSetType(val);
end;

function TMainForm.FindRootType( const val : String ) : String;
var
  tmp : TPascalItem;
  current : String;
begin
  current := val;
  while True do
    begin
      if Copy( UpperCase(current), 1, 5 ) = 'TYPE ' then
        Delete( current, 1, 5 );
      if IsBasicType(current) then
        Break
      else
        begin
          tmp := TPascalItem(Symbols.Find( current ));
          if Assigned(tmp) and (tmp is TNewType) then
            current := TNewType(tmp).TypeRenamed
          else
            Break;
        end;
    end;
  if current <> '' then
    Result := current
  else
    Result := val;
end;

procedure TMainForm.FindConstructors( cl : TMyClass; list : TStrings );

  procedure DoFindConstructors( cl : TMyClass );
  var
    j : Integer;
    o : TObject;
    constr : TConstructor;
  begin
    with cl do
      begin
        for j := 0 to PublicList.Count - 1 do
          if TObject(PublicList.Items[j]) is TConstructor then
            begin
              constr := TConstructor(PublicList.Items[j]);
              if list.IndexOf( constr.Name ) < 0 then
                list.AddObject( constr.Name, constr );
            end;
        if (ParentClasses.Count > 0) then
          o := Symbols.Find( ParentClasses.Strings[0] )
        else if cl.Name <> 'TObject' then
          o := Symbols.Find( 'TObject' )
        else
          o := nil;
        if Assigned(o) and (o is TMyClass) then
          DoFindConstructors( TMyClass(o) );
      end;
  end;

begin
  list.Clear;
  DoFindConstructors( cl );
end;

function TMainForm.FindConstructor( cl : TMyClass ) : TConstructor;
var
  j : Integer;
  o : TObject;
begin
  Result := nil;
  with cl do
    begin
      for j := 0 to PublicList.Count - 1 do
        if TObject(PublicList.Items[j]) is TConstructor then
          begin
            Result := TConstructor(PublicList.Items[j]);
            Break;
          end;
      if not Assigned(Result) then
        begin
          if (ParentClasses.Count > 0) then
            o := Symbols.Find( ParentClasses.Strings[0] )
          else
            o := Symbols.Find( 'TObject' );
          if Assigned(o) and (o is TMyClass) then
            Result := FindConstructor( TMyClass(o) );
        end;
    end;
end;

procedure TMainForm.FillListsWithArgs( args : TArgs );
var
  k, l : Integer;
begin
  FList1.Clear;
  FList2.Clear;
  FList3.Clear;
  FList4.Clear;
  if not Assigned(args) then
    Exit;
  with Args do
    for k := 0 to Items.Count - 1 do
      with TObject(Items.Items[k]) as TArg do
        for l := 0 to Items.Count - 1 do
          begin
            FList1.Add( Items.Strings[l] );
            FList2.Add( Name );
            if IsVar then
              FList3.Add( 'var' )
            else
              FList3.Add( '' );
            FList4.Add( DefaultValue );
          end;
end;

function TMainForm.FindArgsOfConstructor( c : TConstructor ) : TArgs;
begin
  Result := c.Args as TArgs;
  if Assigned(Result) then
    FillListsWithArgs( Result );
end;

function TMainForm.FindOriginalProperty( cl : TMyClass; prop : TProperty ) : TProperty;

  function FindProp( list : TList; const propName : String ) : TProperty;
  var
    i : Integer;
    tmp : TPascalItem;
  begin
    Result := nil;
    for i := 0 to list.Count - 1 do
      begin
        tmp := TPascalItem(list.Items[i]);
        if (tmp.Name = propName) and (tmp is TProperty) then
          begin
            Result := TProperty(tmp);
            Exit;
          end;
      end;
  end;

var
  tmp : TPascalItem;
  parent : String;
begin
  Result := prop;
  if not Assigned(prop) or not Assigned(cl) then Exit;
  if prop.ReturnType <> '' then Exit;
  parent := GetParentClass(cl);
  if parent = '' then Exit;
  tmp := TPascalItem( Symbols.Find( parent ) );
  if not Assigned(tmp) or not (tmp is TMyClass) then
    begin
      Result := nil;
      Exit;
    end;
  cl := TMyClass(tmp);
  Result := FindProp( cl.PublishedList, prop.Name );
  if not Assigned(Result) then
    Result := FindProp( cl.PublicList, prop.Name );
  if not Assigned(Result) then
    Result := FindProp( cl.ProtectedList, prop.Name );
  if not Assigned(Result) then
    Result := FindProp( cl.PrivateList, prop.Name );
  if Assigned(Result) then
    Result := FindOriginalProperty( cl, Result )
  else
    Result := FindOriginalProperty( cl, prop );
end;

function TMainForm.FindUnit( const typeName : String ) : TUnit;
var
  tmp : TPascalItem;
begin
  tmp := TPascalItem( Symbols.Find( typeName ) );
  if Assigned(tmp) then
    Result := tmp.RefUnit
  else
    Result := nil;
end;

function TMainForm.GetParentClass( cl : TMyClass ) : String;
begin
  with cl do
    if ParentClasses.Count > 0 then
      Result := ParentClasses.Strings[0]
    else if CompareText( cl.Name, 'TObject' ) <> 0 then
      Result := 'TObject'
    else
      Result := '';
end;

function TMainForm.IsInterval( p : TPascalItem ) : Boolean;
begin
  Result := Assigned(p) and (p is TNewType) and (TNewType(p).TypeRenamed = '');
end;

function TMainForm.IsSymbolExcluded( const symbol : String ) : Boolean;
var
  i : Integer;
begin
  with Settings.meExclusion.Lines do
    begin
      Result := False;
      for i := 0 to Count - 1 do
        if CompareText( symbol, Strings[i] ) = 0 then
          begin
            Result := True;
            Break;
          end;
    end;
end;

procedure ClearLog;
var
  F : TextFile;
begin
  AssignFile( F, ExtractFilePath(Application.ExeName)+'log.txt');
  Rewrite(F);
  CloseFile(F);
end;

procedure Log( const text : String );
var
  F : TextFile;
begin
  AssignFile( F, ExtractFilePath(Application.ExeName)+'log.txt');
  Append(F);
  try
    WriteLn( F, text );
  finally
    CloseFile(F);
  end;
end;

procedure TMainForm.ListBox1DblClick(Sender: TObject);
var
  s : String;
begin
  with ListBox1 do
    begin
      s := Items.Strings[ItemIndex];
      if InputQuery( 'Edit', 'Unit path:', s ) then
        Items.Strings[ItemIndex] := s;
    end;
end;

end.
